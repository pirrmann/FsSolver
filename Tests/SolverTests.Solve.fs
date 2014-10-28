module ``Solve completely`` 

open NUnit.Framework
open FsUnit

open FsSolver

let Const c = Expression.Value(c, Constant)

let [<Test>] ``Solving a single-step problem yields the results`` () =
    let rules = [ LocalVar "x" =@= Const 1M ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [ Local "x", 1M])

let [<Test>] ``Solving an unsolvable problem yields no result`` () =
    let rules = [ LocalVar "x" =@= LocalVar "y" ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal rules
    newBindings |> should equal Map.empty

let [<Test>] ``Solving a two-step problem yields the results`` () =
    let rules =
        [
            LocalVar "y" =@= LocalVar "x" + Const 1M
            LocalVar "z" =@= LocalVar "y" * Const 2M
        ] |> Set.ofList

    let bindings =
        [
            Local "x", -0.5M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [Local "x", -0.5M
                                             Local "y", 0.5M
                                             Local "z", 1M])

let [<Test>] ``Solving a three-step problem yields the results`` () =
    let rules =
        [
            LocalVar "z" =@= LocalVar "w" / Const 10M
            LocalVar "y" =@= LocalVar "x" + Const 1M
            LocalVar "z" =@= LocalVar "y" * Const 2M
        ] |> Set.ofList

    let bindings =
        [
            Local "x", -0.5M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [Local "x", -0.5M
                                             Local "y", 0.5M
                                             Local "z", 1M
                                             Local "w", 10M])

let [<Test>] ``Solving stops when it can't solve more`` () =
    let rules =
        [
            LocalVar "a" =@= LocalVar "b"
            LocalVar "y" =@= LocalVar "x" + Const 1M
            LocalVar "z" =@= LocalVar "y" * Const 2M
        ] |> Set.ofList

    let bindings =
        [
            Local "x", -0.5M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal (Set.ofList [LocalVar "a" =@= LocalVar "b"])
    newBindings |> should equal (Map.ofList [Local "x", -0.5M
                                             Local "y", 0.5M
                                             Local "z", 1M])
