﻿namespace FsSolver

open FsSolver.Rules

type Problem = {
    Rules: Set<Expression * Expression>
    Bindings: Map<Variable, Value>
    Links: Set<Link> } with
    static member Create (rules, ?bindings) = {
        Rules = rules |> Set.ofSeq
        Bindings = defaultArg bindings Map.empty
        Links = rules |> Seq.collect Links.Extract |> Set.ofSeq }

module Solver =

    let rec private replaceValues values expression =
        match expression with
        | Expression.Var(id) as v ->
            match Map.tryFind id values with
            | Some(Provided(_, _, _) as p) -> Expression.Value(p)
            | Some(Constant _ as value)
            | Some(Computed(_, _) as value) -> Expression.Value(Computed(value.Evaluated, v))
            | Some(Incoherent(_, _))
            | None -> v
        | Expression.UnaryNode(op, e) ->
            Expression.UnaryNode(op, replaceValues values e)
        | Expression.BinaryNode(op, e1, e2) ->
            Expression.BinaryNode(op, replaceValues values e1, replaceValues values e2)
        | Expression.Value _ -> expression

    let rec internal simplify expression =
        match expression with
        | Expression.UnaryNode(op, e) ->
            let se = simplify e
            match se with
            | Expression.Value(v) ->
                Expression.Value(Computed(op.ToOperator v.Evaluated, expression))
            | _ -> Expression.UnaryNode(op, se)
        | Expression.BinaryNode(op, e1, e2) ->
            let se1, se2 = simplify e1, simplify e2
            match se1, se2 with
            | Expression.Value(v1), Expression.Value(v2) ->
                Expression.Value(Computed(op.ToOperator v1.Evaluated v2.Evaluated, expression))
            | _ -> Expression.BinaryNode(op, se1, se2)
        | _ -> expression

    let rec private getExistingBindings expression = seq {
        match expression with
        | Expression.UnaryNode(_, e) ->
            yield! getExistingBindings e
        | Expression.BinaryNode(_, e1, e2) ->
            yield! getExistingBindings e1
            yield! getExistingBindings e2

        | Expression.Value(Provided(_, id, _) as v)
        | Expression.Value(Computed(_, Expression.Var id) as v) -> yield id, v

        | Expression.Value(Computed(_, e)) ->
            yield! getExistingBindings e
        | _ -> () }

    let private getAllIncoherences e incoherency =
        Expressions.getVariablesInComputedValues e
        |> Seq.map (fun id -> id, Incoherent(e, incoherency))

    let private checkIncoherencies eq = seq {
        match eq with
        | Expression.Value v1, Expression.Value v2 ->
            match v1, v2 with
            | Incoherent(_, _), Incoherent(_, _) -> ()
            | Incoherent(e, _), varContainer
            | varContainer, Incoherent(e, _) ->
                match varContainer with
                | Constant _
                | Provided _ -> ()
                | Computed(_, e) -> yield! getAllIncoherences e Propagated
                | Incoherent(_, _) -> failwith "Incoherences should be matched before this case"

            | Constant _, Constant _ -> invalidArg "eq" "This rule either always true or always false, therefore useless"

            | v1, v2 ->
                let value1 = Expression.Value v1
                let value2 = Expression.Value v2
                if v1.Evaluated <> v2.Evaluated then
                    let conflict = Conflict [value1; value2]
                    yield! getAllIncoherences value1 conflict
                    yield! getAllIncoherences value2 conflict
                else
                    yield! getExistingBindings value1
                    yield! getExistingBindings value2
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
                    | Operator.Addition -> Some(n1, Expression.Value(v - v2))
                    | Operator.Substraction -> Some(n1, Expression.Value(v + v2))
                    | Operator.Product -> if v2.Evaluated <> 0M then Some(n1, Expression.Value(v / v2)) else None
                    | Operator.Division -> if v2.Evaluated <> 0M then Some(n1, Expression.Value(v * v2)) else None
                    | Operator.MinOf
                    | Operator.MaxOf -> None
                | Expression.Value v2, _ ->
                    match op with
                    | Operator.Addition -> Some(n2, Expression.Value(v - v2))
                    | Operator.Substraction -> Some(n2, Expression.Value(v2 - v))
                    | Operator.Product -> if v2.Evaluated <> 0M then Some(n2, Expression.Value(v / v2)) else None
                    | Operator.Division -> failwith "The variable must be in the numerator"
                    | Operator.MinOf
                    | Operator.MaxOf -> None
                | _ -> failwith "There should be a value on one side"

            match newEquality with
            | Some eq -> isolateSingleVariable eq
            | None -> Seq.empty
        | _ -> Seq.empty
    
    let private tryIsolateVariable = function
        | (e1, e2) as eq ->
            let allVariablesIds =
                seq {
                    yield! Expressions.getVariablesIds e1
                    yield! Expressions.getVariablesIds e2
                } |> Seq.toList

            match allVariablesIds.Length with
            | 0 -> checkIncoherencies eq
            | 1 -> isolateSingleVariable eq
            | _ -> Seq.empty

    let internal step problem =
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

        // check that we have a single value for each variable
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

        // isolate variables that do not propagate incoherencies
        let neverConflictingVariables =
            problem.Bindings
            |> Seq.choose (fun kvp -> match kvp.Value with | Provided(_, id, ConflictHandlingStrategy.Ignore) -> Some id | _ -> None)
            |> Set.ofSeq
    
        let unifiedNewBindings =
            useFullNewBindings
            |> Seq.map snd
            |> Seq.filter (fun (id, _) -> not(neverConflictingVariables.Contains(id)))
            |> Seq.groupBy fst
            |> Seq.map (fun (id, values) -> unifyValues id (values |> Seq.map snd))

        // add the newly discovered bindings to the map of bindings
        let addBinding bindings (id, value) = bindings |> Map.add id value 
        let allBindings =
            unifiedNewBindings
            |> Seq.fold addBinding problem.Bindings

        // propagate incoherencies
        let rec propagate acc incoherentVariables =
            let newVariables =
                (seq {
                    for incoherentVariable in incoherentVariables do
                    for link in problem.Links do
                    if link.Tips.Contains(incoherentVariable) then
                        yield! link.Tips.Remove(incoherentVariable) |> Seq.filter (fun id -> not(neverConflictingVariables.Contains(id)))
                } |> Set.ofSeq) - acc
            if newVariables.IsEmpty then acc
            else
                propagate (acc + newVariables) newVariables

        let newIncoherencies =
            unifiedNewBindings
            |> Seq.choose (fun (id, value) -> match value with | Incoherent(_, _) -> Some id | _ -> None)
            |> Set.ofSeq

        let propagatedIncoherencies =
            (propagate Set.empty newIncoherencies) - newIncoherencies
            |> Seq.choose (fun id ->
                            match allBindings.TryFind id with
                            | Some(Incoherent(_, _)) // already incoherent
                            | Some(Provided(_, _, ConflictHandlingStrategy.Ignore)) -> None
                            | Some(Constant(v))
                            | Some(Provided(v, _, _))
                            | Some(Computed(v, _)) ->
                                Some(id, Incoherent(ComputedValue(v, Expression.Var id), Propagated))
                            | None ->
                                Some (id, Incoherent(Expression.Var id, Propagated)))
  
        let allBindingsWithPropagatedIncoherencies =
            propagatedIncoherencies
            |> Seq.fold addBinding allBindings

        { problem with
            Rules = remainingRules
            Bindings = allBindingsWithPropagatedIncoherencies
        }

    let rec solve problem =
        let newProblem = step problem
        if newProblem = problem
            then newProblem
            else solve newProblem
