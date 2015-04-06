module ``Report back incoherencies`` 

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``A variable can't have 2 values`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "x" =@= ConstValue 1M
                LocalVar "x" =@= ConstValue 2M
            ])

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Incoherent(LocalVar "x", Conflict([ConstValue 1M; ConstValue 2M]))])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``Equality of a bound variable to the same value causes no conflict`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "x" =@= ConstValue 1M
            ],
            [
                Local "x", Constant 1M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings |> should equal problem.Bindings
    newProblem.Links |> should equal Set.empty

let [<Test>] ``Equality of a bound variable to another value causes a conflict`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "x" =@= ConstValue 2M
            ],
            [
                Local "x", Constant 1M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x", Incoherent(ComputedValue(1M, LocalVar "x"), Conflict([ComputedValue(1M, LocalVar "x"); ConstValue 2M]))])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``Equality of two different variables causes a conflict`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "x" =@= LocalVar "y"
            ],
            [
                Local "x", Constant 1M
                Local "y", Constant 2M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x", Incoherent(ComputedValue(1M, LocalVar "x"), Conflict([ComputedValue(1M, LocalVar "x"); ComputedValue(2M, LocalVar "y")]))
                                     Local "y", Incoherent(ComputedValue(2M, LocalVar "y"), Conflict([ComputedValue(1M, LocalVar "x"); ComputedValue(2M, LocalVar "y")]))])
    newProblem.Links |> should equal (Set.ofList [Link(Local "x", Local "y")])

let [<Test>] ``A false relation including two different variables causes a conflict`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "x" + LocalVar "y" =@= ConstValue 1M 
            ],
            [
                Local "x", Constant 1M
                Local "y", Constant 2M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x", Incoherent(ComputedValue(3M, ComputedValue(1M, LocalVar "x") + ComputedValue(2M, LocalVar "y")), Conflict([ComputedValue(3M, ComputedValue(1M, LocalVar "x") + ComputedValue(2M, LocalVar "y")); ConstValue 1M]))
                                     Local "y", Incoherent(ComputedValue(3M, ComputedValue(1M, LocalVar "x") + ComputedValue(2M, LocalVar "y")), Conflict([ComputedValue(3M, ComputedValue(1M, LocalVar "x") + ComputedValue(2M, LocalVar "y")); ConstValue 1M]))])
    newProblem.Links |> should equal (Set.ofList [Link(Local "x", Local "y")])

let [<Test>] ``Incoherencies are back propagated to all variables - level 1`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "x" =@= ConstValue 1M
                LocalVar "y" =@= ConstValue 2M
                LocalVar "z" =@= ConstValue 2M
                LocalVar "x" =@= LocalVar "y"
                LocalVar "y" =@= LocalVar "z"
            ])

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x", Incoherent(ComputedValue(1M, LocalVar "x"), Conflict([ComputedValue(1M, LocalVar "x"); ComputedValue(2M, LocalVar "y")]))
                                     Local "y", Incoherent(ComputedValue(2M, LocalVar "y"), Conflict([ComputedValue(1M, LocalVar "x"); ComputedValue(2M, LocalVar "y")]))
                                     Local "z", Incoherent(ComputedValue(2M, LocalVar "z"), Propagated)])
    newProblem.Links |> should equal (Set.ofList [Link(Local "x", Local "y")
                                                  Link(Local "y", Local "z")])

let [<Test>] ``Incoherencies are back propagated to all variables - level 2`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "x" =@= ConstValue 1M
                LocalVar "y" =@= LocalVar "x" + ConstValue 1M
                LocalVar "z" =@= LocalVar "y" + ConstValue 1M
                LocalVar "z" =@= LocalVar "y" - ConstValue 1M
            ])

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x", Incoherent(ComputedValue(1M, LocalVar "x"), Propagated)
                                     Local "y", Incoherent(ComputedValue(2M, LocalVar "y"), Propagated)
                                     Local "z", Incoherent(LocalVar "z", Conflict([ComputedValue(2M, LocalVar "y") - ConstValue 1M; ComputedValue(2M, LocalVar "y") + ConstValue 1M]))])
    newProblem.Links |> should equal (Set.ofList [Link(Local "x", Local "y")
                                                  Link(Local "y", Local "z")])
