module ConcretizerTests.ConcretizeRule

open NUnit.Framework
open FsUnit

open FsSolver
open FsSolver.Rules

let [<Test>] ``Concretization of an equality concretizes both nodes`` () =
    let rule = Var "x" === Const 1M
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> should equal (Set.ofList [ScopedVar ["root"; "x"] =@= Expression.Const 1M])

let [<Test>] ``Concretization in rules is applied to all children`` () =
    let rule = ForAllChildren(Var "x" === Const 1M)
    let scope = { Scope.Named "root" with Children = [Scope.Named "child1"
                                                      Scope.Named "child2"] }
    let concreteRule = Concretizer.concretizeRule scope rule |> Set.ofSeq

    concreteRule |> should equal (Set.ofList [ScopedVar ["root"; "child1"; "x"] =@= Expression.Const 1M
                                              ScopedVar ["root"; "child2"; "x"] =@= Expression.Const 1M])

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

    concreteRule |> should equal (Set.ofList [ScopedVar ["root"; "child1"; "baby1"; "x"] =@= Expression.Const 1M
                                              ScopedVar ["root"; "child1"; "baby2"; "x"] =@= Expression.Const 1M
                                              ScopedVar ["root"; "child2"; "baby1"; "x"] =@= Expression.Const 1M
                                              ScopedVar ["root"; "child2"; "baby2"; "x"] =@= Expression.Const 1M
                                              ScopedVar ["root"; "child2"; "baby3"; "x"] =@= Expression.Const 1M])
