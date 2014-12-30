module CSharpTestsRules

open FsSolver
open FsSolver.Rules

type Settings = {
    UseTotalExecFees: bool
}

let GetRules settings = seq {
        //weighted delta
        yield ForAllChildren(!"BaseSize" === Min(Abs(!"Size")))
        yield ForAllChildren(!"WeightedDelta" * !"BaseSize" === Σ(!"Delta" * !"Size"))
        
        yield !"SomePropertyWithoutSetter" === Const 1M

        //exec fees
        yield ForAllChildren(!"ExecFees" === Σ(!"Size") * ParentVar "FeesPerLot")
        if settings.UseTotalExecFees then
            yield Var "TotalExecFees" === Σ(!"ExecFees")
    }
