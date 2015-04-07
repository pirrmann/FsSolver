module ``Solve completely`` 

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``Solving a single-step problem yields the results`` () =
    let problem =
        Problem.Create([ LocalVar "x" =@= ConstValue 1M ])

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Constant 1M])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``Solving an unsolvable problem yields no result`` () =
    let problem =
        Problem.Create([ LocalVar "x" =@= LocalVar "y"])

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal problem.Rules
    newProblem.Bindings |> should equal Map.empty
    newProblem.Links |> should equal (Set.ofList [Link(Local "x", Local "y")])

let [<Test>] ``Solving a two-step problem yields the results`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "y" =@= LocalVar "x" + ConstValue 1M
                LocalVar "z" =@= LocalVar "y" * ConstValue 2M
            ],
            [
                Local "x" |> ProvidedWith -0.5M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x" |> ProvidedWith -0.5M
                                     Local "y", Computed(0.5M, ProvidedValue(-0.5M, Local "x") + ConstValue 1M)
                                     Local "z", Computed(1M, ComputedValue(0.5M, LocalVar "y") * ConstValue 2M)])
    newProblem.Links |> should equal (Set.ofList [Link(Local "x", Local "y")
                                                  Link(Local "y", Local "z")])

let [<Test>] ``Solving a three-step problem yields the results`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "z" =@= LocalVar "w" / ConstValue 10M
                LocalVar "y" =@= LocalVar "x" + ConstValue 1M
                LocalVar "z" =@= LocalVar "y" * ConstValue 2M
            ],
            [
                Local "x" |> ProvidedWith -0.5M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x" |> ProvidedWith -0.5M
                                     Local "y", Computed(0.5M, ProvidedValue(-0.5M, Local "x") + ConstValue 1M)
                                     Local "z", Computed(1M, ComputedValue(0.5M, LocalVar "y") * ConstValue 2M)
                                     Local "w", Computed(10M, ComputedValue(1M, LocalVar "z") * ConstValue 10M)])
    newProblem.Links |> should equal (Set.ofList [Link(Local "x", Local "y")
                                                  Link(Local "y", Local "z")
                                                  Link(Local "w", Local "z")])

let [<Test>] ``Solving stops when it can't solve more`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "a" =@= LocalVar "b"
                LocalVar "y" =@= LocalVar "x" + ConstValue 1M
                LocalVar "z" =@= LocalVar "y" * ConstValue 2M
            ],
            [
                Local "x" |> ProvidedWith -0.5M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal (Set.ofList [LocalVar "a" =@= LocalVar "b"])
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x" |> ProvidedWith -0.5M
                                     Local "y", Computed(0.5M, ProvidedValue(-0.5M, Local "x") + ConstValue 1M)
                                     Local "z", Computed(1M, ComputedValue(0.5M, LocalVar "y") * ConstValue 2M)])
    newProblem.Links |> should equal (Set.ofList [Link(Local "x", Local "y")
                                                  Link(Local "y", Local "z")
                                                  Link(Local "a", Local "b")])
