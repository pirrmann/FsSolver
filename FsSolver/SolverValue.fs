namespace FsSolver

[<RequireQualifiedAccess>]
type SolverValue =
    | Provided of decimal * ConflictHandlingStrategy
    | Computed of decimal * Expression
    | Conflict of Expression array
    | PropagatedIncoherence with
    static member op_Implicit(value) = Provided(value, Propagate)
    static member ProvidedNoConflict(value) = Provided(value, Ignore)
    static member FromValue(value) =
        match value with
        | Value.Provided(v, _, chs) -> Provided(v, chs)
        | Value.Computed(v, e) -> Computed(v, e)
        | Value.Constant(v) as c -> Computed(v, Expression.Value c)
        | Value.Incoherent(_, Incoherence.Conflict(exprs)) -> Conflict(List.toArray exprs)
        | Value.Incoherent(_, Incoherence.Propagated) -> PropagatedIncoherence
    member this.BoundTo(var) =
        match this with
        | Provided(value, conflictHandlingStrategy) -> Value.Provided(value, var, conflictHandlingStrategy)
        | Computed(value, e) -> Value.Computed(value, e)
        | Conflict(exprs) -> Value.Incoherent(Expression.Var var, Incoherence.Conflict(List.ofArray exprs))
        | PropagatedIncoherence -> Value.Incoherent(Expression.Var var, Incoherence.Propagated)