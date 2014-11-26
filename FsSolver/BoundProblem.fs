namespace FsSolver

open FsSolver.Rules

module ReflectedBinders =
    let internal typeAsScope o = o.GetType().Name

    let rec private getBinders name (data:obj) = seq {
        if data <> null then
            let t = data.GetType()
            for pi in t.GetProperties() do
                if pi.PropertyType = typeof<decimal> || pi.PropertyType = typeof<System.Nullable<decimal>> then
                    yield Scoped(name, Local pi.Name), GetterSetter.FromProperty(pi, data)
                else
                    let seqType =
                        pi.PropertyType.GetInterfaces()
                        |> Seq.tryFind (fun i -> i = typeof<System.Collections.IEnumerable>
                                                 || (i.IsGenericType && i.GetGenericTypeDefinition() = typedefof<System.Collections.Generic.IEnumerable<_>>))
                    match seqType with
                    | Some i ->
                        yield!
                            pi.GetMethod.Invoke(data, [||]) :?> seq<_>
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

    static member private Create(rules, scope, binders: (Variable * GetterSetter) seq) =
        {
            Binders = binders |> Map.ofSeq
            Problem =
            {
                Rules =
                    rules
                    |> Seq.collect (Concretizer.concretizeRule scope)
                    |> Set.ofSeq
                Bindings =
                    binders
                    |> Seq.choose (fun (var, gs) -> gs.Get() |> Option.map(fun value -> var, Constant value))
                    |> Map.ofSeq
            }
        }

    static member Create(rules, data) =
        let scope, binders = ReflectedBinders.getScopeAndBinders (ReflectedBinders.typeAsScope data) data
        BoundProblem.Create(rules, scope, binders)

    static member Create(rules, scope, binders: Binder seq) =
        let binders = binders |> Seq.map (fun (Binder(var, gs)) -> var, gs)
        BoundProblem.Create(rules, scope, binders)

    member p.Solve() =
        let solvedProblem = p.Problem |> Solver.solve
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
                | v ->
                    match p.Binders.TryFind binding.Key with
                    | Some binder ->
                        if not (p.Problem.Bindings.ContainsKey binding.Key) then
                            binder.Set(v.Evaluated)
                    | None -> () // ignore intermediate unbound value
        } |> Seq.toArray