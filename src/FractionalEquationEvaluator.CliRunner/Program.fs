namespace FractionalEquattionEvaluator.CliRunnner

open FractionalEquationEvaluator
open FractionalEquationEvaluator.Expression

module Program =

  let rec evaluateUserInput() =
    printf "? "
    let input = System.Console.ReadLine()
    match input.ToLower() with
    | "quit"  -> ()
    | _       ->
      input
      |> Parser.tryParse
      |> Option.map Evaluator.evaluate
      |> Option.map (Number.toString >> sprintf "= %s")
      |> Option.defaultValue "Expression is not valid"
      |> printfn "%s"
      evaluateUserInput()

  let printOpeningDialogue() =
    printfn "Welcome to the Fractional Equation Evaluator!"
    printfn "This program has the following input constraints:"
    printfn "\t- Legal operators are *, /, +, - (multiply, divide, add, subtract)"
    printfn "\t- Operands and operators must be separated by one or more spaces"
    printfn "\t- Mixed numbers must be represented as whole_numerator/denominator. e.g. \"3_1/4\""
    printfn "\t- Improper fractions and whole numbers are also allowed as operands"
    printfn ""
    printfn "Enter \"quit\" at any time to exit the program."
    printfn "Enjoy!"
    printfn ""

  [<EntryPoint>]
  let main _ =
    printOpeningDialogue()
    evaluateUserInput()
    printfn "Thanks! Goodbye!"
    0 // return an integer exit code
