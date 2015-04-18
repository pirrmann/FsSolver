namespace FsSolver

open FsSolver.Rules

module ReflectedBinders =
    let internal typeAsScope o = o.GetType().Name

    let rec private getBinders name (data:obj) = seq {
        if data <> null then
            let t = data.GetType()
            for pi in t.GetProperties() do
                if pi.PropertyType = typeof<decimal> || pi.PropertyType = typeof<System.Nullable<decimal>> then
                    yield Scoped(name, Local pi.Name), DecimalGetterSetter(DecimalGetterSetter.FromProperty(pi, data))
                elif pi.PropertyType = typeof<SolverValue> then
                    yield Scoped(name, Local pi.Name), SolverValueGetterSetter(SolverValueGetterSetter.FromProperty(pi, data))
                else
                    let seqType =
                        pi.PropertyType.GetInterfaces()
                        |> Seq.tryFind (fun i -> i = typeof<System.Collections.IEnumerable>
                                                 || (i.IsGenericType && i.GetGenericTypeDefinition() = typedefof<System.Collections.Generic.IEnumerable<_>>))
                    match seqType with
                    | Some i ->
                        yield!
                            pi.GetGetMethod().Invoke(data, [||]) :?> seq<_>
                            |> Seq.mapi (fun i child -> getBinders (sprintf "%s%d" (typeAsScope child) (i+1)) child)
                            |> Seq.collect (Seq.map (fun (v, gs) -> (Scoped(name, v), gs)))
                    | None -> () }

    let rec private buildScopes variables=
        variables
        |> Seq.choose (function Scoped(name, v) -> Some (name, v) | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (name, children) -> { Scope.Named name with Children = children |> Seq.map snd |> buildScopes |> Seq.toList })

    let private buildScope = buildScopes >> Seq.head

    let getScopeAndBinders name data =
        let binders = getBinders name data |> Seq.toList
        let scope = binders |> List.map fst |> buildScope
        scope, binders

type BoundProblem = {
    Binders: Map<Variable, GetterSetter>
    Problem: Problem } with

    static member private Create(rules, scope, binders: (Variable * GetterSetter) seq, useComputedValues) =
        let getSolverValue (var, gs:GetterSetter) =
            match gs.Get() with
            | Some(SolverValue.Provided(value, _)) -> Some(var, Value.Provided(value, var, Propagate))
            | Some(SolverValue.ProvidedNoConflict(value)) -> Some(var, Value.Provided(value, var, Ignore))
            | Some(SolverValue.Computed(value, expr)) when useComputedValues -> Some(var, Value.Computed(value, expr))
            | _ -> None

        {
            Binders = binders |> Map.ofSeq
            Problem =
                Problem.Create(
                    rules
                    |> Seq.collect (Concretizer.concretizeRule scope)
                    |> Set.ofSeq,
                    binders
                    |> Seq.choose getSolverValue                                        
                    |> Map.ofSeq
            )
        }

    static member Create(rules, data) =
        BoundProblem.Create(rules, data, false)

    static member Create(rules, data, useComputedValues) =
        let scope, binders = ReflectedBinders.getScopeAndBinders (ReflectedBinders.typeAsScope data) data
        BoundProblem.Create(rules, scope, binders, useComputedValues)

    member p.Solve() =
        let solvedProblem = p.Problem |> Solver.solve

        // Mutate the bound problem!
        for binding in solvedProblem.Bindings do
            match p.Binders.TryFind binding.Key with
            | Some binder ->
                match binder.Set with
                | Some setter ->
                    match binding.Value, p.Problem.Bindings.TryFind binding.Key with
                    | Incoherent (_, incoherence), Some(previousValue) ->
                        match previousValue with
                        | Value.Provided(_, _, Ignore)
                        | Value.Constant(_) -> ()
                        | Value.Provided(d, _, Propagate) ->
                            SolverValue.Provided(d, Some incoherence) |> setter
                        | Value.Computed(_, _)
                        | Value.Incoherent(_) ->
                            SolverValue.Incoherent(incoherence) |> setter
                    | v, _ -> SolverValue.FromValue v |> setter
                | None -> () // don't try to set values without setters
            | None -> () // ignore intermediate unbound value

        // Yield the list of conflicts
        seq {
            for binding in solvedProblem.Bindings do
                match binding.Value with
                | Incoherent (expr, incoherence) ->
                    yield
                        binding.Key.ToString(),
                        match incoherence with
                        | Propagated -> [| "Propagated from another conflict" |]
                        | Conflict conflictingExpressions ->
                            conflictingExpressions
                            |> Seq.map (fun e -> e.ToString())
                            |> Seq.toArray
                | _ -> ()
        } |> Seq.toArray