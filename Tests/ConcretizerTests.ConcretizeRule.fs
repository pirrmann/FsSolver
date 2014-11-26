module ``Concretize rules``

open NUnit.Framework
open FsUnit

open FsSolver
open FsSolver.Rules

let [<Test>] ``Concretization of an equality concretizes both nodes`` () =
    let rule = Var "x" === Const 1M
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> should equal (Set.ofList [ScopedVar ["x"; "root"] =@= ConstValue 1M])

let [<Test>] ``Concretization in rules is applied to all children`` () =
    let rule = ForAllChildren(Var "x" === Const 1M)
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> should equal (Set.ofList [ScopedVar ["x"; "child1"; "root"] =@= ConstValue 1M
                                              ScopedVar ["x"; "child2"; "root"] =@= ConstValue 1M])

let [<Test>] ``Concretization can be applied in grand-children rules`` () =
    let rule = ForAllChildren(ForAllChildren(Var "x" === Const 1M))
    let scope = { Scope.Named "root" with
                    Children = [{ Scope.Named "child1"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"] }
                                { Scope.Named "child2"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"
                                                     Scope.Named "baby3"] }] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> should equal (Set.ofList [ScopedVar ["x"; "baby1"; "child1"; "root"] =@= ConstValue 1M
                                              ScopedVar ["x"; "baby2"; "child1"; "root"] =@= ConstValue 1M
                                              ScopedVar ["x"; "baby1"; "child2"; "root"] =@= ConstValue 1M
                                              ScopedVar ["x"; "baby2"; "child2"; "root"] =@= ConstValue 1M
                                              ScopedVar ["x"; "baby3"; "child2"; "root"] =@= ConstValue 1M])

let [<Test>] ``Concretization handles outer scope variales in ForAllChildren rules`` () =
    let rule = ForAllChildren(Var "z" === Var "y" * ParentVar "x")
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> should equal (Set.ofList [ScopedVar ["z"; "child1"; "root"] =@= ScopedVar ["y"; "child1"; "root"] * ScopedVar ["x"; "root"]
                                              ScopedVar ["z"; "child2"; "root"] =@= ScopedVar ["y"; "child2"; "root"] * ScopedVar ["x"; "root"]])
