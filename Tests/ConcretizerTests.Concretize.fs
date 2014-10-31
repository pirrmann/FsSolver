module ``Concretize nodes``

open NUnit.Framework
open FsUnit

open FsSolver
open FsSolver.Rules

let ConstValue c = Expression.Value(Constant c)

let [<Test>] ``A constant expression is not changed`` () =
    let node = Const 1M
    let scope = Scope.Named "root"
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (ConstValue 1M)

let [<Test>] ``An expression without any Sum nor variable is not changed`` () =
    let node = Const 1M + Const 2M
    let scope = Scope.Named "root"
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (ConstValue 1M + ConstValue 2M)

let [<Test>] ``Variables in an expression are scoped`` () =
    let node = (Var "x" / Const 3M +  Const 2M * Var "y") + Var "z"
    let scope = Scope.Named "root"
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (ScopedVar ["root"; "x"] / ConstValue 3M +  ConstValue 2M * ScopedVar ["root"; "y"] + ScopedVar ["root"; "z"])

let [<Test>] ``Sum is replaced by its concrete value for a single child`` () =
    let node = Sum (Const 1M)
    let scope = { Scope.Named "root" with Children = [Scope.Named "child"] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (ConstValue 1M)

let [<Test>] ``Sum is replaced by its concrete value for several children`` () =
    let node = Sum (Const 1M)
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"
                                                      Scope.Named "child3"] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (ConstValue 1M + ConstValue 1M + ConstValue 1M)

let [<Test>] ``Children variables are scoped in the conrete sum (single leg)`` () =
    let node = Sum (Var "x")
    let scope = { Scope.Named "root" with Children = [Scope.Named "child"] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (ScopedVar ["root"; "child"; "x"])

let [<Test>] ``Children variables are scoped in the concrete sum (several legs)`` () =
    let node = Sum (Var "x")
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (ScopedVar ["root"; "child1"; "x"] +
                                ScopedVar ["root"; "child2"; "x"])

let [<Test>] ``Children variables are scoped in the concrete sum for composite expressions`` () =
    let node = Sum (Const 1M + (Var "x" * (Var "y" / Const 2M)))
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (
        (ConstValue 1M + (ScopedVar ["root"; "child1"; "x"] * (ScopedVar ["root"; "child1"; "y"] / ConstValue 2M))) +
        (ConstValue 1M + (ScopedVar ["root"; "child2"; "x"] * (ScopedVar ["root"; "child2"; "y"] / ConstValue 2M))))

let [<Test>] ``Grand-children variables are scoped in the concrete sum`` () =
    let node = Sum(Sum(Var "x"))
    let scope = { Scope.Named "root" with
                    Children = [{ Scope.Named "child1"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"] }
                                { Scope.Named "child2"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"
                                                     Scope.Named "baby3"] }] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (
        (ScopedVar ["root"; "child1"; "baby1"; "x"] + ScopedVar ["root"; "child1"; "baby2"; "x"]) +
        (ScopedVar ["root"; "child2"; "baby1"; "x"] + ScopedVar ["root"; "child2"; "baby2"; "x"] + ScopedVar ["root"; "child2"; "baby3"; "x"]))

let [<Test>] ``Complex expression with variables and constants at every level is concretized`` () =
    let node =
        Var "x" * Const 4M
        + Sum(
            Var "x" * Const 2M + Const 3M
            - (
                Const 0.5M *
                Sum(
                    Var "x" / Const 100M - Const 1M
                )
            )
        )
    let scope = { Scope.Named "root" with
                    Children = [{ Scope.Named "child1"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"] }
                                { Scope.Named "child2"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"
                                                     Scope.Named "baby3"] }] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (
        ScopedVar ["root"; "x"] * ConstValue 4M +
        (
            (
                ScopedVar ["root"; "child1"; "x"] * ConstValue 2M + ConstValue 3M -
                ConstValue 0.5M *
                    (
                        (ScopedVar ["root"; "child1"; "baby1"; "x"] / ConstValue 100M - ConstValue 1M) +
                        (ScopedVar ["root"; "child1"; "baby2"; "x"] / ConstValue 100M - ConstValue 1M)
                    )
            ) +
            (
                ScopedVar ["root"; "child2"; "x"] * ConstValue 2M + ConstValue 3M -
                ConstValue 0.5M *
                    (
                        (ScopedVar ["root"; "child2"; "baby1"; "x"] / ConstValue 100M - ConstValue 1M) +
                        (ScopedVar ["root"; "child2"; "baby2"; "x"] / ConstValue 100M - ConstValue 1M) +
                        (ScopedVar ["root"; "child2"; "baby3"; "x"] / ConstValue 100M - ConstValue 1M)
                    )
            )
        ))
