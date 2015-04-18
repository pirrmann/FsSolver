namespace FsSolver

[<RequireQualifiedAccess>]
type SolverValue =
    | Provided of decimal * Incoherence option
    | ProvidedNoConflict of decimal
    | Computed of decimal * Expression
    | Incoherent of Incoherence
    static member op_Implicit(value) = Provided(value, None)
    static member FromValue(value) =
        match value with
        | Value.Provided(v, _, Propagate) -> Provided(v, None)
        | Value.Provided(v, _, Ignore) -> ProvidedNoConflict(v)
        | Value.Computed(v, e) -> Computed(v, e)
        | Value.Constant(v) as c -> Computed(v, Expression.Value c)
        | Value.Incoherent(_, inc) -> Incoherent(inc)
    member this.AsNullable =
        match this with
        | Provided(v, _)
        | ProvidedNoConflict(v) 
        | Computed(v, _) -> System.Nullable<_>(v)
        | Incoherent(_) -> System.Nullable<_>()
    member this.HasIncoherence =
        match this with
        | Provided(_, Some(_))
        | Incoherent(_) -> true
        | _ -> false
    member this.Description =
        match this with
        | Provided(v, None)
        | ProvidedNoConflict(v) -> sprintf "%M, provided" v
        | Incoherent(inc)
        | Provided(_, Some inc) -> inc.ToString()
        | Computed(_, e) -> e.ToString()