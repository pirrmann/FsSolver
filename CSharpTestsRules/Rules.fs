module CSharpTestsRules

open FsSolver
open FsSolver.Rules

type Settings = {
    UseTotalExecFees: bool
}

let GetRules settings = seq {
        //weighted delta
        yield ForAllChildren(Var "BaseSize" === Min(Abs(Var "Size")))
        yield ForAllChildren(Var "WeightedDelta" * Var "BaseSize" === Sum(Var "Delta" * Var "Size"))
        
        //exec fees
        yield ForAllChildren(Var "ExecFees" === Sum(Var "Size") * ParentVar "FeesPerLot")
        if settings.UseTotalExecFees then
            yield Var "TotalExecFees" === Sum(Var "ExecFees")
    }
