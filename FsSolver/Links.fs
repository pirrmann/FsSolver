namespace FsSolver

open FsSolver.Rules

type Link (x:Variable, y:Variable) =
    member private this.value = if x < y then x, y else y, x
    override x.Equals(yobj) =
        match yobj with
        | :? Link as y -> (x.value = y.value)
        | _ -> false
    override x.GetHashCode() = hash x.value
    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? Link as y -> compare x.value y.value
          | _ -> invalidArg "yobj" "cannot compare values of different types"

module Links =
    let Extract (expr1:Expression, expr2:Expression) =
        let allVariables =
            seq {
                yield! Expressions.getVariablesIds expr1
                yield! Expressions.getVariablesIds expr2    
            } |> Set.ofSeq
        seq {
            for v1 in allVariables do
            for v2 in allVariables do
            if v1 < v2 then yield Link(v1, v2) }

