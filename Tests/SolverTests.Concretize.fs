module SolverTests.Concretize

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``An expression without any Sum nor variable is not changed`` () =
    let expression = Const 1M +  Const 2M
    let scope = Scope.Named "root"
    let concreteRule = expression |> Solver.concretize scope

    concreteRule |> should equal expression

let [<Test>] ``Variables in an expression are scoped`` () =
    let expression = (LocalVar("x") / Const 3M +  Const 2M * LocalVar("y")) + LocalVar "z"
    let scope = Scope.Named "root"
    let concreteRule = expression |> Solver.concretize scope

    concreteRule |> should equal (ScopedVar ["root"; "x"] / Const 3M +  Const 2M * ScopedVar ["root"; "y"] + ScopedVar ["root"; "z"])

let [<Test>] ``Sum is replaced by its concrete value for a single child`` () =
    let expression = Sum (Const 1M)
    let scope = { Scope.Named "root" with Children = [Scope.Named "child"] }
    let concreteRule = expression |> Solver.concretize scope

    concreteRule |> should equal (Const 1M)

let [<Test>] ``Sum is replaced by its concrete value for several children`` () =
    let expression = Sum (Const 1M)
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"
                                                      Scope.Named "child3"] }
    let concreteRule = expression |> Solver.concretize scope

    concreteRule |> should equal (Const 1M + Const 1M + Const 1M)

let [<Test>] ``Children variables are scoped in the conrete sum (single leg)`` () =
    let expression = Sum (LocalVar "x")
    let scope = { Scope.Named "root" with Children = [Scope.Named "child"] }
    let concreteRule = expression |> Solver.concretize scope

    concreteRule |> should equal (ScopedVar ["root"; "child"; "x"])

let [<Test>] ``Children variables are scoped in the concrete sum (several legs)`` () =
    let expression = Sum (LocalVar "x")
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = expression |> Solver.concretize scope

    concreteRule |> should equal (ScopedVar ["root"; "child1"; "x"] +
                                  ScopedVar ["root"; "child2"; "x"])

let [<Test>] ``Children variables are scoped in the concrete sum for composite expressions`` () =
    let expression = Sum (Const 1M + (LocalVar "x" * (LocalVar "y" / Const 2M)))
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = expression |> Solver.concretize scope

    concreteRule |> should equal (
        (Const 1M + (ScopedVar ["root"; "child1"; "x"] * (ScopedVar ["root"; "child1"; "y"] / Const 2M))) +
        (Const 1M + (ScopedVar ["root"; "child2"; "x"] * (ScopedVar ["root"; "child2"; "y"] / Const 2M))))

let [<Test>] ``Grand-children variables are scoped in the concrete sum`` () =
    let expression = Sum(Sum(LocalVar "x"))
    let scope = { Scope.Named "root" with
                    Children = [{ Scope.Named "child1"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"] }
                                { Scope.Named "child2"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"
                                                     Scope.Named "baby3"] }] }
    let concreteRule = expression |> Solver.concretize scope

    concreteRule |> should equal (
        (ScopedVar ["root"; "child1"; "baby1"; "x"] + ScopedVar ["root"; "child1"; "baby2"; "x"]) +
        (ScopedVar ["root"; "child2"; "baby1"; "x"] + ScopedVar ["root"; "child2"; "baby2"; "x"] + ScopedVar ["root"; "child2"; "baby3"; "x"]))

let [<Test>] ``Complex expression with variables and constants at every level is concretized`` () =
    let expression =
        LocalVar "x" * Const 4M
        + Sum(
            LocalVar "x" * Const 2M + Const 3M
            - (
                Const 0.5M *
                Sum(
                    LocalVar "x" / Const 100M - Const 1M
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
    let concreteRule = expression |> Solver.concretize scope

    concreteRule |> should equal (
        ScopedVar ["root"; "x"] * Const 4M +
        (
            (
                ScopedVar ["root"; "child1"; "x"] * Const 2M + Const 3M -
                Const 0.5M *
                    (
                        (ScopedVar ["root"; "child1"; "baby1"; "x"] / Const 100M - Const 1M) +
                        (ScopedVar ["root"; "child1"; "baby2"; "x"] / Const 100M - Const 1M)
                    )
            ) +
            (
                ScopedVar ["root"; "child2"; "x"] * Const 2M + Const 3M -
                Const 0.5M *
                    (
                        (ScopedVar ["root"; "child2"; "baby1"; "x"] / Const 100M - Const 1M) +
                        (ScopedVar ["root"; "child2"; "baby2"; "x"] / Const 100M - Const 1M) +
                        (ScopedVar ["root"; "child2"; "baby3"; "x"] / Const 100M - Const 1M)
                    )
            )
        ))
