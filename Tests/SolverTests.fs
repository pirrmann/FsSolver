module SolverTests

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``Pattern var x = c is promoted to a binding`` () =
    let rules = [ LocalVar "x" === Const 1M ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [ Local "x", 1M])

let [<Test>] ``Pattern c = var x is promoted to a binding`` () =
    let rules = [ Const 2M === LocalVar "y" ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [ Local "y", 2M])

let [<Test>] ``A constant is moved away from the variable side for addition`` () =
    let rules = [ LocalVar("x") + Const(1M) === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Map.empty
    newBindings |> should equal (Map.ofList [ Local "x", 0M ])

let [<Test>] ``A constant is moved away from the variable side for addition - other side`` () =
    let rules = [ Const(1M) + LocalVar("x") === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Map.empty
    newBindings |> should equal (Map.ofList [ Local "x", 0M ])

let [<Test>] ``A constant is moved away from the variable side for substraction`` () =
    let rules = [ LocalVar("x") - Const(1M) === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Map.empty
    newBindings |> should equal (Map.ofList [ Local "x", 2M ])

let [<Test>] ``A constant is moved away from the variable side for substraction - other side`` () =
    let rules = [ Const(1M) - LocalVar("x") === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Map.empty
    newBindings |> should equal (Map.ofList [ Local "x", 0M ])

let [<Test>] ``A constant is moved away from the variable side for product`` () =
    let rules = [ LocalVar("x") * Const(2M) === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Map.empty
    newBindings |> should equal (Map.ofList [ Local "x", 0.5M ])

let [<Test>] ``A constant is moved away from the variable side for product - other side`` () =
    let rules = [ Const(2M) * LocalVar("x") === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Map.empty
    newBindings |> should equal (Map.ofList [ Local "x", 0.5M ])

let [<Test>] ``A constant is moved away from the variable side for division`` () =
    let rules = [ LocalVar("x") / Const(2M) === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Map.empty
    newBindings |> should equal (Map.ofList [ Local "x", 2M ])

let [<Test>] ``A division by a variable is not supported by this solver`` () =
    let rules = [ Const(2M) / LocalVar("x") === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    (fun () -> (rules, bindings) |> step |> ignore) |> should throw typeof<System.Exception>

let [<Test>] ``A variable multiplied by zero can't be solved`` () =
    let rules = [ LocalVar("x") * Const(0M) === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal rules
    newBindings |> should equal Map.empty

let [<Test>] ``A variable divided by zero can't be solved`` () =
    let rules = [ LocalVar("x") / Const(0M) === Const(1M) ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal rules
    newBindings |> should equal Map.empty

let [<Test>] ``Linear relation with a single variable is solved - solve for net`` () =
    let rules =
        [
            LocalVar("net") === LocalVar("gross") * (Const(1M) + LocalVar("execFees") / Const(10000M))
        ] |> Set.ofList

    let bindings =
        [
            Local "gross", 123.02M
            Local "execFees", 2M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [Local "net", 123.044604M
                                             Local "gross", 123.02M
                                             Local "execFees", 2M])

let [<Test>] ``Linear relation with a single variable is solved - solve for gross`` () =
    let rules =
        [
            LocalVar("net") === LocalVar("gross") * (Const(1M) + LocalVar("execFees") / Const(10000M))
        ] |> Set.ofList

    let bindings =
        [
            Local "net", 123.044604M
            Local "execFees", 2M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [Local "net", 123.044604M
                                             Local "gross", 123.02M
                                             Local "execFees", 2M])

let [<Test>] ``Linear relation with a single variable is solved - solve for fees`` () =
    let rules =
        [
            LocalVar("net") === LocalVar("gross") * (Const(1M) + LocalVar("execFees") / Const(10000M))
        ] |> Set.ofList

    let bindings =
        [
            Local "gross", 123.02M
            Local "net", 123.044604M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList [Local "net", 123.044604M
                                             Local "gross", 123.02M
                                             Local "execFees", 2M])


//let rules =
//    [
//        Var("gross") === Var("legGross")
//        Var("net") === Var("gross") * (Const(1M) + Var("execFees") / Const(10000M))
//    ] |> Set.ofList
//
//let bindings =
//    [
//        "legGross", 123.02M
//        "execFees", 2M
//    ] |> Map.ofList
//
//let test = (rules, bindings) |> step |> step
