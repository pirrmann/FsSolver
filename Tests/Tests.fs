module Tests

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``Pattern var x = c is promoted to a binding`` () =
    let rules = [ Var "x" === Const 1M ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList ["x", 1M])

let [<Test>] ``Pattern c = var x is promoted to a binding`` () =
    let rules = [ Const 2M === Var "y" ] |> Set.ofList
    let bindings = Map.empty

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList ["y", 2M])

let [<Test>] ``A constant is moved away from the variable side for addition`` () =
    let rules = [ Var("x") + Const(1M) === Var("y") ] |> Set.ofList
    let bindings = [ "y", 1M ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Map.empty
    newBindings |> should equal (Map.ofList [ "y", 1M
                                              "x", 0M ])

let [<Test>] ``Linear relation with a single variable is solved - solve for net`` () =
    let rules =
        [
            Var("net") === Var("gross") * (Const(1M) + Var("execFees") / Const(10000M))
        ] |> Set.ofList

    let bindings =
        [
            "gross", 123.02M
            "execFees", 2M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList ["net", 123.044604M
                                             "gross", 123.02M
                                             "execFees", 2M])

let [<Test>] ``Linear relation with a single variable is solved - solve for gross`` () =
    let rules =
        [
            Var("net") === Var("gross") * (Const(1M) + Var("execFees") / Const(10000M))
        ] |> Set.ofList

    let bindings =
        [
            "net", 123.044604M
            "execFees", 2M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList ["net", 123.044604M
                                             "gross", 123.02M
                                             "execFees", 2M])

let [<Test>] ``Linear relation with a single variable is solved - solve for fees`` () =
    let rules =
        [
            Var("net") === Var("gross") * (Const(1M) + Var("execFees") / Const(10000M))
        ] |> Set.ofList

    let bindings =
        [
            "gross", 123.02M
            "net", 123.044604M
        ] |> Map.ofList

    let newRules, newBindings = (rules, bindings) |> step

    newRules |> should equal Set.empty
    newBindings |> should equal (Map.ofList ["net", 123.044604M
                                             "gross", 123.02M
                                             "execFees", 2M])


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
