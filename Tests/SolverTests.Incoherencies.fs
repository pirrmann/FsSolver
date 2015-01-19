module ``Report back incoherencies`` 

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``A variable can't have 2 values`` () =
    let problem = {
        Rules =
            [
                LocalVar "x" =@= ConstValue 1M
                LocalVar "x" =@= ConstValue 2M
            ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Incoherent(LocalVar "x", Conflict([ConstValue 1M; ConstValue 2M]))])

let [<Test>] ``Equality of a bound variable to the same value causes no conflict`` () =
    let problem = {
        Rules =
            [
                LocalVar "x" =@= ConstValue 1M
            ] |> Set.ofList
        Bindings =
            [
                Local "x" |> ProvidedWith 1M
            ] |> Map.ofList }

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings |> should equal problem.Bindings

let [<Test>] ``Equality of a bound variable to another value causes a conflict`` () =
    let problem = {
        Rules =
            [
                LocalVar "x" =@= ConstValue 2M
            ] |> Set.ofList
        Bindings =
            [
                Local "x" |> ProvidedWith 1M
            ] |> Map.ofList }

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x", Incoherent(ProvidedValue(1M, Local "x"), Conflict([ProvidedValue(1M, Local "x"); ConstValue 2M]))])

let [<Test>] ``Equality of two different variables causes a conflict`` () =
    let problem = {
        Rules =
            [
                LocalVar "x" =@= LocalVar "y"
            ] |> Set.ofList
        Bindings =
            [
                Local "x" |> ProvidedWith 1M
                Local "y" |> ProvidedWith 2M
            ] |> Map.ofList }

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x", Incoherent(ProvidedValue(1M, Local "x"), Conflict([ProvidedValue(1M, Local "x"); ProvidedValue(2M, Local "y")]))
                                     Local "y", Incoherent(ProvidedValue(2M, Local "y"), Conflict([ProvidedValue(1M, Local "x"); ProvidedValue(2M, Local "y")]))])

let [<Test>] ``A false relation including two different variables causes a conflict`` () =
    let problem = {
        Rules =
            [
                LocalVar "x" + LocalVar "y" =@= ConstValue 1M 
            ] |> Set.ofList
        Bindings =
            [
                Local "x" |> ProvidedWith 1M
                Local "y" |> ProvidedWith 2M
            ] |> Map.ofList }

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x", Incoherent(ComputedValue(3M, ProvidedValue(1M, Local "x") + ProvidedValue(2M, Local "y")), Conflict([ComputedValue(3M, ProvidedValue(1M, Local "x") + ProvidedValue(2M, Local "y")); ConstValue 1M]))
                                     Local "y", Incoherent(ComputedValue(3M, ProvidedValue(1M, Local "x") + ProvidedValue(2M, Local "y")), Conflict([ComputedValue(3M, ProvidedValue(1M, Local "x") + ProvidedValue(2M, Local "y")); ConstValue 1M]))])
