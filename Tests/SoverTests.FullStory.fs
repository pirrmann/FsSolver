module SolverTests.FullStory

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``Weighted delta full story`` () =
    let rules =
        [
            ForAllChildren(LocalVar "baseSize" === Min(LocalVar "size"))
            ForAllChildren(LocalVar "weightedDelta" * LocalVar "baseSize" === Sum(LocalVar "delta" * LocalVar "size"))
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

    let concreteRules = rules |> Seq.collect (Solver.concretizeRule scope) |> Set.ofSeq

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
