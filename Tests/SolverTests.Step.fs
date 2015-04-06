module ``Solve a single step``

open NUnit.Framework
open FsUnit

open FsSolver

let Var = Expression.Var

let [<Test>] ``Pattern var x = c is promoted to a binding`` () =
    let problem =
        Problem.Create([ LocalVar "x" =@= ConstValue 1M ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Constant 1M])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``Pattern c = var x is promoted to a binding`` () =
    let problem =
        Problem.Create([ ConstValue 2M =@= LocalVar "y" ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "y", Constant 2M])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``A constant is moved away from the variable side for addition`` () =
    let problem =
        Problem.Create([ LocalVar("x") + ConstValue(1M) =@= ConstValue(1M) ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0M, ConstValue 1M - ConstValue 1M)])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``A constant is moved away from the variable side for addition - other side`` () =
    let problem =
        Problem.Create([ ConstValue(1M) + LocalVar("x") =@= ConstValue(1M) ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0M, ConstValue 1M - ConstValue 1M)])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``A constant is moved away from the variable side for substraction`` () =
    let problem =
        Problem.Create([LocalVar("x") - ConstValue(1M) =@= ConstValue(1M) ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(2M, ConstValue 1M + ConstValue 1M)])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``A constant is moved away from the variable side for substraction - other side`` () =
    let problem =
        Problem.Create([ ConstValue(1M) - LocalVar("x") =@= ConstValue(1M) ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0M, ConstValue 1M - ConstValue 1M)])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``A constant is moved away from the variable side for product`` () =
    let problem =
        Problem.Create([ LocalVar("x") * ConstValue(2M) =@= ConstValue(1M) ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0.5M, ConstValue 1M / ConstValue 2M)])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``A constant is moved away from the variable side for product - other side`` () =
    let problem =
        Problem.Create([ ConstValue(2M) * LocalVar("x") =@= ConstValue(1M) ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0.5M, ConstValue 1M / ConstValue 2M)])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``A constant is moved away from the variable side for division`` () =
    let problem = 
        Problem.Create([ LocalVar("x") / ConstValue(2M) =@= ConstValue(1M) ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(2M, ConstValue 1M * ConstValue 2M)])
    newProblem.Links |> should equal Set.empty

let [<Test>] ``A division by a variable is not supported by this solver`` () =
    let problem =
        Problem.Create([ ConstValue(2M) / LocalVar("x") =@= ConstValue(1M) ])

    (fun () -> problem |> Solver.step |> ignore) |> should throw typeof<System.Exception>

let [<Test>] ``A variable multiplied by zero can't be solved`` () =
    let problem =
        Problem.Create([ LocalVar("x") * ConstValue(0M) =@= ConstValue(1M) ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal problem.Rules
    newProblem.Bindings |> should equal Map.empty
    newProblem.Links |> should equal Set.empty

let [<Test>] ``A variable divided by zero can't be solved`` () =
    let problem =
        Problem.Create([ LocalVar("x") / ConstValue(0M) =@= ConstValue(1M) ])

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal problem.Rules
    newProblem.Bindings |> should equal Map.empty
    newProblem.Links |> should equal Set.empty

let [<Test>] ``Linear relation with a single variable is solved - solve for net`` () =
    let problem =
        Problem.Create(
            [ LocalVar("net") =@= LocalVar("gross") * (ConstValue(1M) + LocalVar("execFees") / ConstValue(10000M)) ],
            [
                Local "gross", Constant 123.02M
                Local "execFees", Constant 2M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "net", Computed(123.044604M, ComputedValue(123.02M, LocalVar "gross") * (ConstValue 1M + ComputedValue(2M, LocalVar "execFees") / ConstValue 10000M))
                                     Local "gross", Constant 123.02M
                                     Local "execFees", Constant 2M])
    newProblem.Links
        |> should equal (Set.ofList [Link(Local "net", Local "gross")
                                     Link(Local "net", Local "execFees")
                                     Link(Local "execFees", Local "gross")])

let [<Test>] ``Linear relation with a single variable is solved - solve for gross`` () =
    let problem =
        Problem.Create(
            [ LocalVar("net") =@= LocalVar("gross") * (ConstValue(1M) + LocalVar("execFees") / ConstValue(10000M)) ],
            [
                Local "net", Constant 123.044604M
                Local "execFees", Constant 2M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "net", Constant 123.044604M
                                     Local "gross", Computed(123.02M, ComputedValue(123.044604M, LocalVar "net") / (ConstValue 1M + ComputedValue(2M, LocalVar "execFees") / ConstValue 10000M))
                                     Local "execFees", Constant 2M])
    newProblem.Links
        |> should equal (Set.ofList [Link(Local "net", Local "gross")
                                     Link(Local "net", Local "execFees")
                                     Link(Local "execFees", Local "gross")])

let [<Test>] ``Linear relation with a single variable is solved - solve for fees`` () =
    let problem =
        Problem.Create(
            [ LocalVar("net") =@= LocalVar("gross") * (ConstValue(1M) + LocalVar("execFees") / ConstValue(10000M)) ],
            [
                Local "gross", Constant 123.02M
                Local "net", Constant 123.044604M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "net", Constant 123.044604M
                                     Local "gross", Constant 123.02M
                                     Local "execFees", Computed(2M, (ComputedValue(123.044604M, LocalVar "net") / ComputedValue(123.02M, LocalVar "gross") - ConstValue 1M) * ConstValue 10000M)])
    newProblem.Links
        |> should equal (Set.ofList [Link(Local "net", Local "gross")
                                     Link(Local "net", Local "execFees")
                                     Link(Local "execFees", Local "gross")])

let [<Test>] ``New bindings are injected in the rules in 2nd step`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "y" =@= LocalVar "x" + ConstValue 1M
                LocalVar "z" =@= LocalVar "y" * ConstValue 2M
            ],
            [
                Local "x", Constant -0.5M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.step |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x", Constant -0.5M
                                     Local "y", Computed(0.5M, ComputedValue(-0.5M, LocalVar "x") + ConstValue 1M)
                                     Local "z", Computed(1M, ComputedValue(0.5M, LocalVar "y") * ConstValue 2M)])
    newProblem.Links
        |> should equal (Set.ofList [Link(Local "x", Local "y")
                                     Link(Local "y", Local "z")])

let [<Test>] ``Variables in different scopes are not mixed up`` () =
    let problem =
        Problem.Create(
            [
                LocalVar "net" =@= Var(Scoped("leg1", Local("net"))) + Var(Scoped("leg2", Local("net")))
            ],
            [
                Local "net", Constant 2M
                Scoped("leg1", Local("net")), Constant 1M
            ] |> Map.ofList)

    let newProblem = problem |> Solver.step |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "net", Constant 2M
                                     Scoped("leg1", Local("net")), Constant 1M
                                     Scoped("leg2", Local("net")), Computed(1M, ComputedValue(2M, LocalVar "net") - ComputedValue(1M, ScopedVar ["net"; "leg1"]))])
    newProblem.Links
        |> should equal (Set.ofList [Link(Local "net", Scoped("leg1", Local("net")))
                                     Link(Local "net", Scoped("leg2", Local("net")))
                                     Link(Scoped("leg1", Local("net")), Scoped("leg2", Local("net")))])
