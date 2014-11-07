namespace FsSolver

type Problem = {
    Rules: Set<Expression * Expression>
    Bindings: Map<Variable, Value> }

module Solver =

    let rec private replaceValues values expression =
        match expression with
        | Expression.Var(id) as v ->
            match Map.tryFind id values with
            | Some(value:Value) -> Expression.Value(Computed(value.Evaluated, v))
            | None -> v
        | Expression.BinaryNode(op, e1, e2) ->
            Expression.BinaryNode(op, replaceValues values e1, replaceValues values e2)
        | Expression.Value _ -> expression

    let rec private simplify expression =
        match expression with
        | Expression.BinaryNode(op, e1, e2) ->
            let se1, se2 = simplify e1, simplify e2
            match se1, se2 with
            | Expression.Value(v1), Expression.Value(v2) ->
                Expression.Value(Computed(op.ToOperator v1.Evaluated v2.Evaluated, expression))
            | _ -> Expression.BinaryNode(op, se1, se2)
        | _ -> expression

    let rec private getVariablesIds expression = seq {
        match expression with
        | Expression.BinaryNode(_, e1, e2) ->
            yield! getVariablesIds e1
            yield! getVariablesIds e2
        | Expression.Var id -> yield id
        | _ -> () }

    let private hasVariable = getVariablesIds >> Seq.isEmpty >> not

    let rec private getVariablesInComputedValues expression = seq {
        match expression with
        | Expression.BinaryNode(_, e1, e2) ->
            yield! getVariablesInComputedValues e1
            yield! getVariablesInComputedValues e2
        | Expression.Value(Computed(_, e)) ->
            yield! getVariablesInComputedValues e
        | Expression.Var id -> yield id
        | _ -> () }

    let rec private getExistingBindings expression = seq {
        match expression with
        | Expression.BinaryNode(_, e1, e2) ->
            yield! getExistingBindings e1
            yield! getExistingBindings e2
        | Expression.Value(Computed(_, Expression.Var v) as c) -> yield v, c
        | Expression.Value(Computed(_, e)) ->
            yield! getExistingBindings e
        | _ -> () }

    let private getAllIncoherences e incoherency =
        getVariablesInComputedValues e
        |> Seq.map (fun id -> id, Incoherent(e, incoherency))

    let private checkIncoherencies eq = seq {
        match eq with
        | Expression.Value v1, Expression.Value v2 ->
            match v1, v2 with
            | Incoherent(_, _), Incoherent(_, _) -> ()
            | Incoherent(e, _), varContainer
            | varContainer, Incoherent(e, _) ->
                match varContainer with
                | Constant _ -> ()
                | Computed(_, e) -> yield! getAllIncoherences e Propagated
                | Incoherent(_, _) -> failwith "Incoherences should be matched before this case"
            | Constant(c), Computed(d, e)
            | Computed(d, e), Constant(c) ->
                let cv = ComputedValue(d, e)
                if d <> c then
                    yield! getAllIncoherences cv (Conflict([cv; ConstValue c]))
                else
                    yield! getExistingBindings cv
            | Computed(d1, e1), Computed(d2, e2) ->
                let cv1 = ComputedValue(d1, e1)
                let cv2 = ComputedValue(d2, e2)
                if d1 <> d2 then
                    let conflict = Conflict [cv1; cv2]
                    yield! getAllIncoherences cv1 conflict
                    yield! getAllIncoherences cv2 conflict
                else
                    yield! getExistingBindings cv1
                    yield! getExistingBindings cv2
            | Constant _, Constant _ -> invalidArg "eq" "This rule either always true or always false, therefore useless"
        | _ -> invalidArg "eq" "The equality should contain values on both sides" }

    let rec private isolateSingleVariable eq =
        let varSide, v =
            match eq with 
            | e, Expression.Value(v)
            | Expression.Value(v), e -> e, v
            | _ -> failwith "There should be a value on one side"

        match varSide with
        | Expression.Var id ->
            match v with
            | Computed(value, Expression.Var _) as identifiedVar ->
                Seq.singleton (id, Computed(value, Expression.Value identifiedVar))
            | _ ->
                Seq.singleton (id, v)

        | Expression.BinaryNode(op, n1, n2) ->
            let newEquality =
                match n1, n2 with
                | _, Expression.Value v2 ->
                    match op with
                    | Addition -> Some(n1, Expression.Value(v - v2))
                    | Substraction -> Some(n1, Expression.Value(v + v2))
                    | Product -> if v2.Evaluated <> 0M then Some(n1, Expression.Value(v / v2)) else None
                    | Division -> if v2.Evaluated <> 0M then Some(n1, Expression.Value(v * v2)) else None
                    | MinOf -> None
                | Expression.Value v2, _ ->
                    match op with
                    | Addition -> Some(n2, Expression.Value(v - v2))
                    | Substraction -> Some(n2, Expression.Value(v2 - v))
                    | Product -> if v2.Evaluated <> 0M then Some(n2, Expression.Value(v / v2)) else None
                    | Division -> failwith "The variable must be in the numerator"
                    | MinOf -> None
                | _ -> failwith "There should be a value on one side"

            match newEquality with
            | Some eq -> isolateSingleVariable eq
            | None -> Seq.empty
        | _ -> Seq.empty
    
    let private tryIsolateVariable = function
        | (e1, e2) as eq ->
            let allVariablesIds =
                seq {
                    yield! getVariablesIds e1
                    yield! getVariablesIds e2
                } |> Seq.toList

            match allVariablesIds.Length with
            | 0 -> checkIncoherencies eq
            | 1 -> isolateSingleVariable eq
            | _ -> Seq.empty

    let step problem =
        // inject the bound values in variables and simplify
        let injectAndSimplify = (replaceValues problem.Bindings) >> simplify
        let simplifiedRules = problem.Rules |> Set.map (fun (x, y) -> injectAndSimplify x, injectAndSimplify y)

        // try to infer new bindings
        let newBindings =
            simplifiedRules
            |> Seq.collect (fun eq -> tryIsolateVariable eq |> Seq.map (fun c -> eq, c))
            |> Seq.toList

        // remove the rules where there is nothing more to solve
        let remainingRules =
            simplifiedRules - (newBindings |> Seq.map fst |> Set.ofSeq)
        
        // remove the useless bindings that were just needed to get rid of the rules (to be refactored)
        let useFullNewBindings =
            newBindings
            |> Seq.filter (function | _, (v, Computed(_, Expression.Var v')) when v = v' -> false | _ -> true)

        let unifyValues id values =
            match values |> Seq.tryPick (fun v -> match v with | Incoherent(_, _) as i -> Some(i) | _ -> None) with
            | Some i -> id, i
            | None ->
                match values |> Seq.distinctBy (fun v -> v.Evaluated) |> Seq.toList with
                | [v] -> id, v
                | _ -> id, Incoherent(
                                Expression.Var id,
                                values
                                    |> Seq.map (fun v -> v.Expression)
                                    |> Seq.toList
                                    |> Conflict)

        let unifiedNewBindings =
            useFullNewBindings
            |> Seq.map snd
            |> Seq.groupBy fst
            |> Seq.map (fun (id, values) -> unifyValues id (values |> Seq.map snd))

        // add the newly discovered bindings to the map of bindings
        let addBinding bindings (id, value) = bindings |> Map.add id value 
        let allBindings =
            unifiedNewBindings
            |> Seq.fold addBinding problem.Bindings
    
        {
            Rules = remainingRules
            Bindings = allBindings
        }

    let rec solve problem =
        let newProblem = step problem
        if newProblem = problem
            then newProblem
            else solve newProblem
