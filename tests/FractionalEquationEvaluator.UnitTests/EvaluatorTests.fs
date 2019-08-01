namespace FractionalEquationEvaluator

open Xunit
open FsUnit.Xunit

module EvaluatorTests =

  open Expression

  [<Fact>]
  let ``1/8 + 1_3/4 equals 1_7/8``() =
    let lhs = Operand <| Fraction (1, 8)
    let rhs = Operand <| Mixed (1, 3, 4)
    let eq = Operation (Add, lhs, rhs)
    let expected = Mixed (1, 7, 8)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected

  [<Fact>]
  let ``1 + 3/4 equals 1_3/4``() =
    let lhs = Operand <| Whole (1)
    let rhs = Operand <| Fraction (3, 4)
    let eq = Operation (Add, lhs, rhs)
    let expected = Mixed (1, 3, 4)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected

  [<Fact>]
  let ``1 + 6/4 equals 2_1/2``() =
    let lhs = Operand <| Whole (1)
    let rhs = Operand <| Fraction (6, 4)
    let eq = Operation (Add, lhs, rhs)
    let expected = Mixed (2, 1, 2)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected

  [<Fact>]
  let ``1_3/4 - 1/8 equals 1_5/8``() =
    let lhs = Operand <| Mixed (1, 3, 4)
    let rhs = Operand <| Fraction (1, 8)
    let eq = Operation (Subtract, lhs, rhs)
    let expected = Mixed (1, 5, 8)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected

  [<Fact>]
  let ``1/2 * 3_3/4 equals 1_7/8``() =
    let lhs = Operand <| Fraction (1, 2)
    let rhs = Operand <| Mixed (3, 3, 4)
    let eq = Operation (Multiply, lhs, rhs)
    let expected = Mixed (1, 7, 8)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected

  [<Fact>]
  let ``3 * 3/4 equals 2_1/4``() =
    let lhs = Operand <| Whole 3
    let rhs = Operand <| Fraction (3, 4)
    let eq = Operation (Multiply, lhs, rhs)
    let expected = Mixed (2, 1, 4)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected

  [<Fact>]
  let ``2/3 / 3/4 equals 8/9``() =
    let lhs = Operand <| Fraction (2, 3)
    let rhs = Operand <| Fraction (3, 4)
    let eq = Operation (Divide, lhs, rhs)
    let expected = Fraction (8, 9)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected

  [<Fact>]
  let ``1/8 + 1/8 + 1/8 equals 3/8``() =
    let first = Operand <| Fraction (1, 8)
    let second = Operand <| Fraction (1, 8)
    let third = Operand <| Fraction (1, 8)
    let eq = Operation (Add, (Operation (Add, first, second)), third)
    let expected = Fraction (3, 8)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected

  [<Fact>]
  let ``3/8 - 1/8 - 1/8 equals 1/8``() =
    let first = Operand <| Fraction (3, 8)
    let second = Operand <| Fraction (1, 8)
    let third = Operand <| Fraction (1, 8)
    let eq = Operation (Subtract, (Operation (Subtract, first, second)), third)
    let expected = Fraction (1, 8)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected

  [<Fact>]
  let ``3/8 - 1/8 + 1/8 equals 3/8``() =
    let first = Operand <| Fraction (3, 8)
    let second = Operand <| Fraction (1, 8)
    let third = Operand <| Fraction (1, 8)
    let eq = Operation (Subtract, (Operation (Subtract, first, second)), third)
    let expected = Fraction (1, 8)

    let actual = Evaluator.evaluate eq

    actual |> should equal expected