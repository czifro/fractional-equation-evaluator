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

  type Equation =
    | Operand of Number
    | Operation of (Operator * Equation * Equation)

[<RequireQualifiedAccess>]
module Parser =

  open System.Text.RegularExpressions
  open Expression

  type internal EquationToken =
    | NumberToken of Number
    | OperatorToken of Operator

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

[<RequireQualifiedAccess>]
module Evaluator =

  open Expression

  let evaluate (equation:Equation) : Number =
    let performOperation (op:Operator) (lhs:Number) (rhs:Number) =
      let addSub (op:int -> int -> int) (lhs:Number) (rhs:Number) : Number =
        let addSub' (whole:int, num:int, denom:int) (number:Number) : Number =
          match number with
          | Whole w -> Mixed (op whole w, num, denom)
          | Mixed (w, n, d) -> Mixed (op whole w, op (num * d) (denom * n), denom * d)
          | Fraction (n, d) -> Mixed (whole, op (num * d) (denom * n), denom * d)

        match lhs with
        | Whole whole                -> addSub' (whole, 0, 1) rhs
        | Mixed (whole, num, denom)  -> addSub' (whole, num, denom) rhs
        | Fraction (num, denom)      -> addSub' (0, num, denom) rhs

      let multDiv (isMult:bool) (lhs:Number) (rhs:Number) : Number =
        let mult (num:int, denom:int) (rhs:Number) : Number =
          match rhs with
          | Whole w -> Fraction (num * w, denom)
          | Mixed (w, n, d) -> Fraction (num * (w * d + n), denom * d)
          | Fraction (n, d) -> Fraction (num * n, denom * d)

        let div (num:int, denom:int) (rhs:Number) : Number =
          match rhs with
          | Whole w -> Fraction (num * w, denom)
          | Mixed (w, n, d) -> Fraction (num * d, denom * (w * d + n))
          | Fraction (n, d) -> Fraction (num * d, denom * n)

        match lhs, isMult with
        | Whole whole, true   -> mult (whole, 1) rhs
        | Whole whole, false  -> div (whole, 1) rhs
        | Mixed (whole, num, denom), true   -> mult (whole * denom + num, denom) rhs
        | Mixed (whole, num, denom), false  -> div (whole * denom + num, denom) rhs
        | Fraction (num, denom), true   -> mult (num, denom) rhs
        | Fraction (num, denom), false  -> div (num, denom) rhs

      match op with
      | Add       -> addSub (+) lhs rhs
      | Subtract  -> addSub (-) lhs rhs
      | Multiply  -> multDiv true lhs rhs
      | Divide    -> multDiv false lhs rhs

    let rec eval (eq:Equation) (cont:Number -> Number) : Number =
      match eq with
      | Operand num               -> cont <| num
      | Operation (op, eq0, eq1)  ->
        eval eq0 (fun lhs ->
          eval eq1 (fun rhs ->
            cont <| performOperation op lhs rhs
          )
        )

    let normalize (number:Number) : Number =
      let reduceFraction (num:int, denom:int) : int * int =
        let rec gcd a b =
          match abs b with
          | 0 -> a
          | _ -> gcd b (a % b)

        let gcd' = gcd num denom
        (num / gcd', denom / gcd')

      let convertImproperFractionToMixed (num:int, denom:int) : int * int * int =
        let whole = num / denom
        let remainder = num % denom
        (whole, remainder, denom)

      let normalizeMixed (whole:int, num:int, denom:int) =
        let (whole', num, denom) =
          if num > denom then convertImproperFractionToMixed (num, denom)
          else (0, num, denom)
        let (num, denom) = reduceFraction (num, denom)
        match whole + whole', num, denom with
        | 0, 0, _ -> Whole 0
        | w, n, 1 -> Whole (w + n)
        | 0, n, d -> Fraction (n, d)
        | w, n, d -> Mixed (w, n, d)

      match number with
      | Whole _ -> number
      | Mixed (whole, num, denom) -> normalizeMixed (whole, num, denom)
      | Fraction (num, denom) -> normalizeMixed (0, num, denom)

    eval equation id
    |> normalize