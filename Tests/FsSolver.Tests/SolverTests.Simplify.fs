module ``Simplify expressions``

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``Absolute values of a positive number is simplified`` () =

    let expression = Expression.UnaryNode(UnaryOperator.Abs, ConstValue 1M)

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(1M, expression))

let [<Test>] ``Absolute values of a negative number is simplified`` () =

    let expression = Expression.UnaryNode(UnaryOperator.Abs, ConstValue -1M)

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(1M, expression))

let [<Test>] ``Sign of a positive number is simplified`` () =

    let expression = Expression.UnaryNode(UnaryOperator.Sign, ConstValue 10M)

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(1M, expression))

let [<Test>] ``Sign of a negative number is simplified`` () =

    let expression = Expression.UnaryNode(UnaryOperator.Sign, ConstValue -10M)

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(-1M, expression))

let [<Test>] ``Sign of 0 is positive 1`` () =

    let expression = Expression.UnaryNode(UnaryOperator.Sign, ConstValue 0M)

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(1M, expression))

let [<Test>] ``Sign of -0 is positive 1`` () =

    let expression = Expression.UnaryNode(UnaryOperator.Sign, ConstValue -0M)

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(1M, expression))

let [<Test>] ``Additions are simplified`` () =

    let expression = ConstValue 1M + ConstValue 1M

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(2M, expression))

let [<Test>] ``Substractions are simplified`` () =

    let expression = ConstValue 1M - ConstValue 1M

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(0M, expression))

let [<Test>] ``Products are simplified`` () =

    let expression = ConstValue 2M * ConstValue 3M

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(6M, expression))

let [<Test>] ``Divisions are simplified`` () =

    let expression = ConstValue 6M / ConstValue 3M

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(2M, expression))

let [<Test>] ``Binary MinOf nodes are simplified`` () =

    let expression = Expression.BinaryNode(Operator.MinOf, ConstValue 6M, ConstValue 3M)

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(3M, expression))

let [<Test>] ``Binary MaxOf nodes are simplified`` () =

    let expression = Expression.BinaryNode(Operator.MaxOf, ConstValue 6M, ConstValue 3M)

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(6M, expression))

let [<Test>] ``Simplification is applied recursively`` () =

    let expression =
        Expression.BinaryNode(Operator.MinOf, ConstValue 1M, ConstValue 2M)
        * Expression.BinaryNode(Operator.MaxOf, ConstValue 5M, ConstValue 3M)
        + ConstValue 2M
        - (ConstValue 1M / ConstValue 2M)

    let simplified = expression |> Solver.simplify

    simplified |> shouldEqual (ComputedValue(6.5M, expression))
