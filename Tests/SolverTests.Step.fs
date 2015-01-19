module ``Solve a single step``

open NUnit.Framework
open FsUnit

open FsSolver

let Var = Expression.Var

let [<Test>] ``Pattern var x = c is promoted to a binding`` () =
    let problem = {
        Rules = [ LocalVar "x" =@= ConstValue 1M ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Constant 1M])

let [<Test>] ``Pattern c = var x is promoted to a binding`` () =
    let problem = {
        Rules = [ ConstValue 2M =@= LocalVar "y" ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "y", Constant 2M])

let [<Test>] ``A constant is moved away from the variable side for addition`` () =
    let problem = {
        Rules = [ LocalVar("x") + ConstValue(1M) =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0M, ConstValue 1M - ConstValue 1M)])

let [<Test>] ``A constant is moved away from the variable side for addition - other side`` () =
    let problem = {
        Rules = [ ConstValue(1M) + LocalVar("x") =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0M, ConstValue 1M - ConstValue 1M)])

let [<Test>] ``A constant is moved away from the variable side for substraction`` () =
    let problem = {
        Rules = [ LocalVar("x") - ConstValue(1M) =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(2M, ConstValue 1M + ConstValue 1M)])

let [<Test>] ``A constant is moved away from the variable side for substraction - other side`` () =
    let problem = {
        Rules = [ ConstValue(1M) - LocalVar("x") =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0M, ConstValue 1M - ConstValue 1M)])

let [<Test>] ``A constant is moved away from the variable side for product`` () =
    let problem = {
        Rules = [ LocalVar("x") * ConstValue(2M) =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0.5M, ConstValue 1M / ConstValue 2M)])

let [<Test>] ``A constant is moved away from the variable side for product - other side`` () =
    let problem = {
        Rules = [ ConstValue(2M) * LocalVar("x") =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(0.5M, ConstValue 1M / ConstValue 2M)])

let [<Test>] ``A constant is moved away from the variable side for division`` () =
    let problem = {
        Rules = [ LocalVar("x") / ConstValue(2M) =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Map.empty
    newProblem.Bindings |> should equal (Map.ofList [Local "x", Computed(2M, ConstValue 1M * ConstValue 2M)])

let [<Test>] ``A division by a variable is not supported by this solver`` () =
    let problem = {
        Rules = [ ConstValue(2M) / LocalVar("x") =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    (fun () -> problem |> Solver.step |> ignore) |> should throw typeof<System.Exception>

let [<Test>] ``A variable multiplied by zero can't be solved`` () =
    let problem = {
        Rules = [ LocalVar("x") * ConstValue(0M) =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal problem.Rules
    newProblem.Bindings |> should equal Map.empty

let [<Test>] ``A variable divided by zero can't be solved`` () =
    let problem = {
        Rules = [ LocalVar("x") / ConstValue(0M) =@= ConstValue(1M) ] |> Set.ofList
        Bindings = Map.empty }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal problem.Rules
    newProblem.Bindings |> should equal Map.empty

let [<Test>] ``Linear relation with a single variable is solved - solve for net`` () =
    let problem = {
        Rules = [ LocalVar("net") =@= LocalVar("gross") * (ConstValue(1M) + LocalVar("execFees") / ConstValue(10000M)) ] |> Set.ofList
        Bindings =
            [
                Local "gross" |> ProvidedWith 123.02M
                Local "execFees" |> ProvidedWith 2M
            ] |> Map.ofList }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "net", Computed(123.044604M, ProvidedValue(123.02M, Local "gross") * (ConstValue 1M + ProvidedValue(2M, Local "execFees") / ConstValue 10000M))
                                     Local "gross" |> ProvidedWith 123.02M
                                     Local "execFees" |> ProvidedWith 2M])

let [<Test>] ``Linear relation with a single variable is solved - solve for gross`` () =
    let problem = {
        Rules = [ LocalVar("net") =@= LocalVar("gross") * (ConstValue(1M) + LocalVar("execFees") / ConstValue(10000M)) ] |> Set.ofList
        Bindings =
            [
                Local "net" |> ProvidedWith 123.044604M
                Local "execFees" |> ProvidedWith 2M
            ] |> Map.ofList }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "net" |> ProvidedWith 123.044604M
                                     Local "gross", Computed(123.02M, ProvidedValue(123.044604M, Local "net") / (ConstValue 1M + ProvidedValue(2M, Local "execFees") / ConstValue 10000M))
                                     Local "execFees" |> ProvidedWith 2M])

let [<Test>] ``Linear relation with a single variable is solved - solve for fees`` () =
    let problem = {
        Rules = [ LocalVar("net") =@= LocalVar("gross") * (ConstValue(1M) + LocalVar("execFees") / ConstValue(10000M)) ] |> Set.ofList
        Bindings =
            [
                Local "gross" |> ProvidedWith 123.02M
                Local "net" |> ProvidedWith 123.044604M
            ] |> Map.ofList }

    let newProblem = problem |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "net" |> ProvidedWith 123.044604M
                                     Local "gross" |> ProvidedWith 123.02M
                                     Local "execFees", Computed(2M, (ProvidedValue(123.044604M, Local "net") / ProvidedValue(123.02M, Local "gross") - ConstValue 1M) * ConstValue 10000M)])

let [<Test>] ``New bindings are injected in the rules in 2nd step`` () =
    let problem = {
        Rules =
            [
                LocalVar "y" =@= LocalVar "x" + ConstValue 1M
                LocalVar "z" =@= LocalVar "y" * ConstValue 2M
            ] |> Set.ofList
        Bindings =
            [
                Local "x" |> ProvidedWith -0.5M
            ] |> Map.ofList }

    let newProblem = problem |> Solver.step |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "x" |> ProvidedWith -0.5M
                                     Local "y", Computed(0.5M, ProvidedValue(-0.5M, Local "x") + ConstValue 1M)
                                     Local "z", Computed(1M, ComputedValue(0.5M, LocalVar "y") * ConstValue 2M)])

let [<Test>] ``Variables in different scopes are not mixed up`` () =
    let problem = {
        Rules =
            [
                LocalVar "net" =@= Var(Scoped("leg1", Local("net"))) + Var(Scoped("leg2", Local("net")))
            ] |> Set.ofList
        Bindings =
            [
                Local "net" |> ProvidedWith 2M
                Scoped("leg1", Local("net")) |> ProvidedWith 1M
            ] |> Map.ofList }

    let newProblem = problem |> Solver.step |> Solver.step

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal (Map.ofList [Local "net" |> ProvidedWith 2M
                                     Scoped("leg1", Local("net")) |> ProvidedWith 1M
                                     Scoped("leg2", Local("net")), Computed(1M, ProvidedValue(2M, Local "net") - ProvidedValue(1M, ScopedVariable ["net"; "leg1"]))])
