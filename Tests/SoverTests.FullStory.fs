﻿module ``Real use cases``

open NUnit.Framework
open FsUnit

open FsSolver
open FsSolver.Rules

let [<Test>] ``Weighted delta`` () =
    let rules =
        [
            ForAllChildren(Var "baseSize" === Min(Var "size"))
            ForAllChildren(Var "weightedDelta" * Var "baseSize" === Sum(Var "delta" * Var "size"))
        ]

    let scope =
        { Scope.Named "rfq" with
            Children =
            [
                { Scope.Named "underlying"
                    with Children = [Scope.Named "leg1"
                                     Scope.Named "leg2"] }
            ]
        }

    let concreteRules = rules |> Seq.collect (Concretizer.concretizeRule scope) |> Set.ofSeq

    let bindings =
        [
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "size"))), 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "delta"))), 10.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "size"))), 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "delta"))), -12.0M
        ] |> Map.ofList

    let newRules, newBindings = (concreteRules, bindings) |> Solver.solve

    newRules |> should equal Set.empty
    newBindings |> should equal
        (Map.ofList [
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "size"))), 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "delta"))), 10.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "size"))), 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "delta"))), -12.0M
            Scoped("rfq", Scoped("underlying", Local "baseSize")), 100.0M
            Scoped("rfq", Scoped("underlying", Local "weightedDelta")), -2.0M
        ])

let [<Test>] ``Exec fees`` () =
    let rules =
        [
            Var "totalExecFees" === Sum(Var "execFees")
            ForAllChildren(Var "execFees" === Sum(Var "size") * ParentVar "feesPerLot")
        ]

    let scope =
        { Scope.Named "rfq" with
            Children =
            [
                { Scope.Named "underlying"
                    with Children = [Scope.Named "leg1"
                                     Scope.Named "leg2"] }
            ]
        }

    let concreteRules = rules |> Seq.collect (Concretizer.concretizeRule scope) |> Set.ofSeq

    let bindings =
        [
            Scoped("rfq", Local "feesPerLot"), 1.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "size"))), 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "size"))), 100.0M
        ] |> Map.ofList

    let newRules, newBindings = (concreteRules, bindings) |> Solver.solve

    newRules |> should equal Set.empty
    newBindings |> should equal
        (Map.ofList [
            Scoped("rfq", Local "feesPerLot"), 1.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "size"))), 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "size"))), 100.0M
            Scoped("rfq", Scoped("underlying", Local "execFees")), 200.0M
            Scoped("rfq", Local "totalExecFees"), 200.0M
        ])
