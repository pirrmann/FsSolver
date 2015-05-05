module ``Concretize rules``

open NUnit.Framework
open FsUnit

open FsSolver
open FsSolver.Rules

let [<Test>] ``Concretization of an equality concretizes both nodes`` () =
    let rule = !"x" === Const 1M
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> shouldEqual (Set.ofList [ScopedVar ["x"; "root"] =@= ConstValue 1M])

let [<Test>] ``Concretization in rules is applied to all children`` () =
    let rule = ForAllChildren(!"x" === Const 1M)
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> shouldEqual (Set.ofList [ScopedVar ["x"; "child1"; "root"] =@= ConstValue 1M
                                             ScopedVar ["x"; "child2"; "root"] =@= ConstValue 1M])

let [<Test>] ``Concretization can be applied in grand-children rules`` () =
    let rule = ForAllChildren(ForAllChildren(!"x" === Const 1M))
    let scope = { Scope.Named "root" with
                    Children = [{ Scope.Named "child1"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"] }
                                { Scope.Named "child2"
                                    with Children = [Scope.Named "baby1"
                                                     Scope.Named "baby2"
                                                     Scope.Named "baby3"] }] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> shouldEqual (Set.ofList [ScopedVar ["x"; "baby1"; "child1"; "root"] =@= ConstValue 1M
                                             ScopedVar ["x"; "baby2"; "child1"; "root"] =@= ConstValue 1M
                                             ScopedVar ["x"; "baby1"; "child2"; "root"] =@= ConstValue 1M
                                             ScopedVar ["x"; "baby2"; "child2"; "root"] =@= ConstValue 1M
                                             ScopedVar ["x"; "baby3"; "child2"; "root"] =@= ConstValue 1M])

let [<Test>] ``Concretization handles outer scope variales in ForAllChildren rules`` () =
    let rule = ForAllChildren(!"z" === !"y" * ParentVar "x")
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> shouldEqual (Set.ofList [ScopedVar ["z"; "child1"; "root"] =@= ScopedVar ["y"; "child1"; "root"] * ScopedVar ["x"; "root"]
                                             ScopedVar ["z"; "child2"; "root"] =@= ScopedVar ["y"; "child2"; "root"] * ScopedVar ["x"; "root"]])

let [<Test>] ``Concretization of First and Last rules`` () =
    let rule = !"z" === First(!"x") - Last(!"y")
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> shouldEqual (Set.ofList [ScopedVar ["z"; "root"] =@= ScopedVar ["x"; "child1"; "root"] - ScopedVar ["y"; "child2"; "root"]])

