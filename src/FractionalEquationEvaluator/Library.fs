namespace FractionalEquationEvaluator

[<AutoOpen>]
module Prelude =

  [<AutoOpen>]
  module Operators =

    let (>>=) o f = Option.bind f o

module Expression =

  type WholeNumber = int

  type Numerator = int

  type Denominator = int

  type Number =
    | Whole of WholeNumber
    | Fraction of (Numerator * Denominator)
    | Mixed of (WholeNumber * Numerator * Denominator)

  type Operator =
    | Add
    | Subtract
    | Multiply
    | Divide

  type internal EquationToken =
    | NumberToken of Number
    | OperatorToken of Operator

  type Equation =
    | Operand of Number
    | Operation of (Operator * Equation * Equation)

[<RequireQualifiedAccess>]
module Parser =

  open System.Text.RegularExpressions
  open Expression

  let tryParse (raw:string) : Equation option =
    let tryParseTokens (rawTokens:string list) : EquationToken list option =
      let parseAsNumber (token:string) : EquationToken option =
        let tryParseWholeNumber() : Number option =
          let wholeNumberFmt = "^(\\d+)$";
          let wholeNumberMatches =
            Regex.Match(token, wholeNumberFmt).Groups
            |> Seq.cast<Group>

          wholeNumberMatches |> Seq.tryItem 1 >>= fun whole ->
            let whole = int whole.Value
            Some <| Whole whole

        let tryParseMixedNumber() : Number option =
          let mixedNumberFmt = "^(\\d+)_(\\d*/\\d*)$";
          let mixedNumberMatches =
            Regex.Match(token, mixedNumberFmt).Groups
            |> Seq.cast<Group>

          mixedNumberMatches |> Seq.tryItem 1 >>= fun whole ->
            let whole = int whole.Value
            mixedNumberMatches |> Seq.tryItem 2 >>= fun frac ->
              let (num, denom) =
                let frac = frac.Value.Split('/')
                int frac.[0], int frac.[1]
              match denom with
              | 0 -> None
              | _ -> Some <| Mixed (whole, num, denom)

        let tryParseFractionNumber() =
          let fractionFmt = "^(\\d+)/(\\d+)$"
          let fractionNumberMatches =
            Regex.Match(token, fractionFmt).Groups
            |> Seq.cast<Group>

          fractionNumberMatches |> Seq.tryItem 1 >>= fun num ->
            fractionNumberMatches |> Seq.tryItem 2 >>= fun denom ->
              let num = int num.Value
              let denom = int denom.Value
              match denom with
              | 0 -> None
              | _ -> Some <| Fraction (num, denom)

        tryParseWholeNumber()
        |> Option.orElseWith tryParseMixedNumber
        |> Option.orElseWith tryParseFractionNumber
        |> Option.map NumberToken

      let parseAsOperator (token:string) : EquationToken option =
        match token with
        | "+" -> Some <| OperatorToken Add
        | "-" -> Some <| OperatorToken Subtract
        | "*" -> Some <| OperatorToken Multiply
        | "/" -> Some <| OperatorToken Divide
        | _   -> None

      let parsedTokens =
        rawTokens
        |> List.map(fun token ->
          token
          |> parseAsNumber
          |> Option.orElse (parseAsOperator token)
        )

      let folder (head:EquationToken option) (tail:EquationToken list option) =
        head >>= (fun h -> tail >>= (fun t -> Some <| h::t))
      List.foldBack folder parsedTokens (Some <| List.empty<EquationToken>)

    let constructEquation (tokens:EquationToken list) : Equation option =
      let rec innerConstructEquation
        (tokens:EquationToken list)
        (cont:Equation -> Equation option)
        : Equation option =
        match tokens with
        | [] | [_; _]  -> None
        | [x]          ->
          match x with
          | OperatorToken _  -> None
          | NumberToken num  -> cont <| Operand num
        | x0::x1::xs   ->
          match x0, x1 with
          | NumberToken _, NumberToken _
          | OperatorToken _, NumberToken _
          | OperatorToken _, OperatorToken _ -> None
          | NumberToken num, OperatorToken op ->
            innerConstructEquation xs (fun x ->
              cont <| Operation (op, Operand num, x)
            )

      innerConstructEquation tokens Some

    raw.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> tryParseTokens
    |> Option.bind constructEquation

