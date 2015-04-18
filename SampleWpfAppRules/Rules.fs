module SampleWpfAppRules

open FsSolver
open FsSolver.Rules

let GetRules () = seq {        
        yield !"Z" === !"X" + !"Y"
    }
