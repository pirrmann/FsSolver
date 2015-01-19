﻿module ``Real use cases``

open NUnit.Framework
open FsUnit

open FsSolver
open FsSolver.Rules

let [<Test>] ``Weighted delta`` () =
    let rules =
        [
            ForAllChildren(!"baseSize" === Min(Abs(!"size")))
            ForAllChildren(!"weightedDelta" * !"baseSize" === Σ(!"delta" * !"size"))
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

    let problem = {
        Rules = rules |> Seq.collect (Concretizer.concretizeRule scope) |> Set.ofSeq
        Bindings =
        [
            ScopedVariable ["size" ; "leg1"; "underlying"; "rfq"] |> ProvidedWith 100.0M
            ScopedVariable ["delta"; "leg1"; "underlying"; "rfq"] |> ProvidedWith 10.0M
            ScopedVariable ["size" ; "leg2"; "underlying"; "rfq"] |> ProvidedWith -100.0M
            ScopedVariable ["delta"; "leg2"; "underlying"; "rfq"] |> ProvidedWith 12.0M
        ] |> Map.ofList }

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal
        (Map.ofList [
            ScopedVariable ["size" ; "leg1"; "underlying"; "rfq"] |> ProvidedWith 100.0M
            ScopedVariable ["delta"; "leg1"; "underlying"; "rfq"] |> ProvidedWith 10.0M
            ScopedVariable ["size" ; "leg2"; "underlying"; "rfq"] |> ProvidedWith -100.0M
            ScopedVariable ["delta"; "leg2"; "underlying"; "rfq"] |> ProvidedWith 12.0M
            ScopedVariable ["baseSize"; "underlying"; "rfq"], Computed(100.0M, Expression.BinaryNode(Operator.MinOf, Expression.UnaryNode(UnaryOperator.Abs, ProvidedValue(100M, ScopedVariable ["size"; "leg1"; "underlying"; "rfq"])), Expression.UnaryNode(UnaryOperator.Abs, ProvidedValue(-100M, ScopedVariable ["size"; "leg2"; "underlying"; "rfq"]))))
            ScopedVariable ["weightedDelta"; "underlying"; "rfq"], Computed(-2.0M, (ProvidedValue(10.0M, ScopedVariable ["delta"; "leg1"; "underlying"; "rfq"]) * ProvidedValue(100M, ScopedVariable ["size"; "leg1"; "underlying"; "rfq"]) + ProvidedValue(12.0M, ScopedVariable ["delta"; "leg2"; "underlying"; "rfq"]) * ProvidedValue(-100M, ScopedVariable ["size"; "leg2"; "underlying"; "rfq"])) / ComputedValue(100M, ScopedVar ["baseSize"; "underlying"; "rfq"])) 
        ])

let [<Test>] ``Exec fees`` () =
    let rules =
        [
            !"totalExecFees" === Σ(!"execFees")
            ForAllChildren(!"execFees" === Σ(!"size") * ParentVar "feesPerLot")
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

    let problem = {
        Rules = rules |> Seq.collect (Concretizer.concretizeRule scope) |> Set.ofSeq
        Bindings =
        [
            ScopedVariable ["feesPerLot"; "rfq"] |> ProvidedWith 1.0M
            ScopedVariable ["size"; "leg1"; "underlying"; "rfq"] |> ProvidedWith 100.0M
            ScopedVariable ["size"; "leg2"; "underlying"; "rfq"] |> ProvidedWith 100.0M
        ] |> Map.ofList }

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal
        (Map.ofList [
            ScopedVariable ["feesPerLot"; "rfq"] |> ProvidedWith 1.0M
            ScopedVariable ["size"; "leg1"; "underlying"; "rfq"] |> ProvidedWith 100.0M
            ScopedVariable ["size"; "leg2"; "underlying"; "rfq"] |> ProvidedWith 100.0M
            ScopedVariable ["execFees"; "underlying"; "rfq"], Computed(200.0M, (ProvidedValue(100.0M, ScopedVariable ["size"; "leg1"; "underlying"; "rfq"]) + ProvidedValue(100.0M, ScopedVariable ["size"; "leg2"; "underlying"; "rfq"])) * ProvidedValue(1.0M, ScopedVariable ["feesPerLot"; "rfq"]))
            ScopedVariable ["totalExecFees"; "rfq"], Computed(200.0M, ComputedValue(200.0M, ScopedVar ["execFees"; "underlying"; "rfq"]))
        ])
