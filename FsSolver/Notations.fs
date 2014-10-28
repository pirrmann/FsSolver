﻿namespace FsSolver

[<AutoOpen>]
module Notations =
    let LocalVar = Local >> Expression.Var

    let ScopedVar names =
        match List.rev names with
        | [] -> failwith "Can't build a scoped variable from a empty list"
        | local :: scope ->
            scope |> List.fold (fun s name -> Scoped(name, s)) (Local local)
        |> Expression.Var

    let (=@=) expr1 expr2 = expr1, expr2
    let (===) expr1 expr2 = Rules.Equality(expr1, expr2)

    let ParentVar name = Rules.OuterScopeVar(name, 1)