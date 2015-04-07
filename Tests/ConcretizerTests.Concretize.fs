module ``Concretize nodes``

open NUnit.Framework
open FsUnit

open FsSolver
open FsSolver.Rules

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

    expression |> should equal (ScopedVar ["x"; "root"] / ConstValue 3M +  ConstValue 2M * ScopedVar ["y"; "root"] + ScopedVar ["z"; "root"])

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

let [<Test>] ``Children variables are scoped in the conrete sum - single leg`` () =
    let node = Sum (Var "x")
    let scope = { Scope.Named "root" with Children = [Scope.Named "child"] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (ScopedVar ["x"; "child"; "root"])

let [<Test>] ``Children variables are scoped in the concrete sum - several legs`` () =
    let node = Sum (Var "x")
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (ScopedVar ["x"; "child1"; "root"] +
                                ScopedVar ["x"; "child2"; "root"])

let [<Test>] ``Children variables are scoped in the concrete sum for composite expressions`` () =
    let node = Sum (Const 1M + (Var "x" * (Var "y" / Const 2M)))
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let expression = node |> Concretizer.concretize scope

    expression |> should equal (
        (ConstValue 1M + (ScopedVar ["x"; "child1"; "root"] * (ScopedVar ["y"; "child1"; "root"] / ConstValue 2M))) +
        (ConstValue 1M + (ScopedVar ["x"; "child2"; "root"] * (ScopedVar ["y"; "child2"; "root"] / ConstValue 2M))))

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
        (ScopedVar ["x"; "baby1"; "child1"; "root"] + ScopedVar ["x"; "baby2"; "child1"; "root"]) +
        (ScopedVar ["x"; "baby1"; "child2"; "root"] + ScopedVar ["x"; "baby2"; "child2"; "root"] + ScopedVar ["x"; "baby3"; "child2"; "root"]))

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
        ScopedVar ["x"; "root"] * ConstValue 4M +
        (
            (
                ScopedVar ["x"; "child1"; "root"] * ConstValue 2M + ConstValue 3M -
                ConstValue 0.5M *
                    (
                        (ScopedVar ["x"; "baby1"; "child1"; "root"] / ConstValue 100M - ConstValue 1M) +
                        (ScopedVar ["x"; "baby2"; "child1"; "root"] / ConstValue 100M - ConstValue 1M)
                    )
            ) +
            (
                ScopedVar ["x"; "child2"; "root"] * ConstValue 2M + ConstValue 3M -
                ConstValue 0.5M *
                    (
                        (ScopedVar ["x"; "baby1"; "child2"; "root"] / ConstValue 100M - ConstValue 1M) +
                        (ScopedVar ["x"; "baby2"; "child2"; "root"] / ConstValue 100M - ConstValue 1M) +
                        (ScopedVar ["x"; "baby3"; "child2"; "root"] / ConstValue 100M - ConstValue 1M)
                    )
            )
        ))
