module ``Real use cases``

open NUnit.Framework
open FsUnit

open FsSolver
open FsSolver.Rules

let [<Test>] ``Weighted delta`` () =
    let rules =
        [
            ForAllChildren(Var "baseSize" === Min(Abs(Var "size")))
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

    let problem = {
        Rules = rules |> Seq.collect (Concretizer.concretizeRule scope) |> Set.ofSeq
        Bindings =
        [
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "size"))), Constant 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "delta"))), Constant 10.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "size"))), Constant -100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "delta"))), Constant 12.0M
        ] |> Map.ofList }

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal
        (Map.ofList [
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "size"))), Constant 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "delta"))), Constant 10.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "size"))), Constant -100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "delta"))), Constant 12.0M
            Scoped("rfq", Scoped("underlying", Local "baseSize")), Computed(100.0M, Expression.BinaryNode(MinOf, Expression.UnaryNode(AbsoluteValue, ComputedValue(100M, ScopedVar ["size"; "leg1"; "underlying"; "rfq"])), Expression.UnaryNode(AbsoluteValue, ComputedValue(-100M, ScopedVar ["size"; "leg2"; "underlying"; "rfq"]))))
            Scoped("rfq", Scoped("underlying", Local "weightedDelta")), Computed(-2.0M, (ComputedValue(10.0M, ScopedVar ["delta"; "leg1"; "underlying"; "rfq"]) * ComputedValue(100M, ScopedVar ["size"; "leg1"; "underlying"; "rfq"]) + ComputedValue(12.0M, ScopedVar ["delta"; "leg2"; "underlying"; "rfq"]) * ComputedValue(-100M, ScopedVar ["size"; "leg2"; "underlying"; "rfq"])) / ComputedValue(100M, ScopedVar ["baseSize"; "underlying"; "rfq"])) 
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

    let problem = {
        Rules = rules |> Seq.collect (Concretizer.concretizeRule scope) |> Set.ofSeq
        Bindings =
        [
            Scoped("rfq", Local "feesPerLot"), Constant 1.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "size"))), Constant 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "size"))), Constant 100.0M
        ] |> Map.ofList }

    let newProblem = problem |> Solver.solve

    newProblem.Rules |> should equal Set.empty
    newProblem.Bindings
        |> should equal
        (Map.ofList [
            Scoped("rfq", Local "feesPerLot"), Constant 1.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg1", Local "size"))), Constant 100.0M
            Scoped("rfq", Scoped("underlying", Scoped("leg2", Local "size"))), Constant 100.0M
            Scoped("rfq", Scoped("underlying", Local "execFees")), Computed(200.0M, (ComputedValue(100.0M, ScopedVar ["size"; "leg1"; "underlying"; "rfq"]) + ComputedValue(100.0M, ScopedVar ["size"; "leg2"; "underlying"; "rfq"])) * ComputedValue(1.0M, ScopedVar ["feesPerLot"; "rfq"]))
            Scoped("rfq", Local "totalExecFees"), Computed(200.0M, ComputedValue(200.0M, ScopedVar ["execFees"; "underlying"; "rfq"]))
        ])
