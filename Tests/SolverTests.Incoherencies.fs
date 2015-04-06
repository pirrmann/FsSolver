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
