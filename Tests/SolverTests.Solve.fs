module ``Solve completely`` 

open NUnit.Framework
open FsUnit

open FsSolver

let ConstValue = Constant >> Expression.Value
let ComputedValue = Computed >> Expression.Value

let [<Test>] ``Solving a single-step problem yields the results`` () =
    let rules = [ LocalVar "x" =@= ConstValue 1M ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [Local "x", Constant 1M])

let [<Test>] ``Solving an unsolvable problem yields no result`` () =
    let rules = [ LocalVar "x" =@= LocalVar "y" ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal rules
    newBindings |> should equal Map.empty

let [<Test>] ``Solving a two-step problem yields the results`` () =
    let rules =
        [
            LocalVar "y" =@= LocalVar "x" + ConstValue 1M
            LocalVar "z" =@= LocalVar "y" * ConstValue 2M
        ] |> Set.ofList

    let bindings =
        [
            Local "x", Constant -0.5M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [Local "x", Constant -0.5M
                                             Local "y", Computed(0.5M, ComputedValue(-0.5M, LocalVar "x") + ConstValue 1M)
                                             Local "z", Computed(1M, ComputedValue(0.5M, LocalVar "y") * ConstValue 2M)])

let [<Test>] ``Solving a three-step problem yields the results`` () =
    let rules =
        [
            LocalVar "z" =@= LocalVar "w" / ConstValue 10M
            LocalVar "y" =@= LocalVar "x" + ConstValue 1M
            LocalVar "z" =@= LocalVar "y" * ConstValue 2M
        ] |> Set.ofList

    let bindings =
        [
            Local "x", Constant -0.5M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [Local "x", Constant -0.5M
                                             Local "y", Computed(0.5M, ComputedValue(-0.5M, LocalVar "x") + ConstValue 1M)
                                             Local "z", Computed(1M, ComputedValue(0.5M, LocalVar "y") * ConstValue 2M)
                                             Local "w", Computed(10M, ComputedValue(1M, LocalVar "z") * ConstValue 10M)])

let [<Test>] ``Solving stops when it can't solve more`` () =
    let rules =
        [
            LocalVar "a" =@= LocalVar "b"
            LocalVar "y" =@= LocalVar "x" + ConstValue 1M
            LocalVar "z" =@= LocalVar "y" * ConstValue 2M
        ] |> Set.ofList

    let bindings =
        [
            Local "x", Constant -0.5M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> Solver.solve

    newRules |> should equal (Set.ofList [LocalVar "a" =@= LocalVar "b"])
    newBindings |> should equal (Map.ofList [Local "x", Constant -0.5M
                                             Local "y", Computed(0.5M, ComputedValue(-0.5M, LocalVar "x") + ConstValue 1M)
                                             Local "z", Computed(1M, ComputedValue(0.5M, LocalVar "y") * ConstValue 2M)])
