namespace FractionalEquationEvaluator

open System
open Xunit
open FsUnit.Xunit

module ParserTests =

  open Expression

  [<Fact>]
  let ``1/2 successfully parses``() =
    let input = "1/2"
    let expected = (1, 2) |> Fraction |> Operand |> Some

    let actual = Parser.tryParse input

    actual |> should equal expected

  [<Fact>]
  let ``1_1/2 successfully parses``() =
    let input = "1_1/2"
    let expected = (1, 1, 2) |> Mixed |> Operand |> Some

    let actual = Parser.tryParse input

    actual |> should equal expected

  [<Fact>]
  let ``1 successfully parses``() =
    let input = "1"
    let expected = 1 |> Whole |> Operand |> Some

    let actual = Parser.tryParse input

    actual |> should equal expected

  [<Fact>]
  let ``1_1/2 + 1/2 successfully parses``() =
    let input = "1_1/2 + 1/2"
    let expected =
      let lhs = (1, 1, 2) |> Mixed |> Operand
      let rhs = (1, 2) |> Fraction |> Operand
      Some <| Operation (Add, lhs, rhs)

    let actual = Parser.tryParse input

    actual |> should equal expected

  [<Fact>]
  let ``1_1/2 + 1/2 + 2 successfully parses``() =
    let input = "1_1/2 + 1/2 + 2"
    let expected =
      let lhs = (1, 1, 2) |> Mixed |> Operand
      let rhs =
        let lhs = (1, 2) |> Fraction |> Operand
        let rhs = 2 |> Whole |> Operand
        Operation (Add, lhs, rhs)
      Some <| Operation (Add, lhs, rhs)

    let actual = Parser.tryParse input

    actual |> should equal expected

  [<Theory>]
  [<InlineData("")>]
  [<InlineData("t")>]
  [<InlineData("_1/2")>]
  [<InlineData("1/0")>]
  [<InlineData("1_1")>]
  [<InlineData("1_1/0")>]
  [<InlineData("1 ++ 2")>]
  [<InlineData("1/2/3")>]
  [<InlineData("1 + + 2")>]
  let ``Bad input fails to parse`` (input:string) =
    let actual = Parser.tryParse input

    actual |> should equal None
