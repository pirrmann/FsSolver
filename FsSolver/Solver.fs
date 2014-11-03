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

    let hasVariable = getVariablesIds >> Seq.isEmpty >> not

    let rec private isolateSingleVariable eq =
        let varSide, v =
            match eq with 
            | e, Expression.Value(v)
            | Expression.Value(v), e -> e, v
            | _ -> failwith "There should be a value on one side"

        match varSide with
        | Expression.Var id ->
            match v with
            | Computed(value, Expression.Var _) as identifiedVar -> Some(id, Computed(value, Expression.Value identifiedVar))
            | _ -> Some(id, v)

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
            newEquality |> Option.bind isolateSingleVariable
        | _ -> None
    
    let private tryIsolateVariable = function
        | (e1, e2) as eq ->
            let allVariablesIds =
                seq {
                    yield! getVariablesIds e1
                    yield! getVariablesIds e2
                } |> Seq.toList

            if allVariablesIds.Length = 1
            then isolateSingleVariable eq
            else None

    let step problem =
        // inject the bound values in variables and simplify
        let injectAndSimplify = (replaceValues problem.Bindings) >> simplify
        let simplifiedRules = problem.Rules |> Set.map (fun (x, y) -> injectAndSimplify x, injectAndSimplify y)

        // try to infer new bindings
        let newBindings =
            simplifiedRules
            |> Seq.choose (fun eq -> tryIsolateVariable eq |> Option.map (fun c -> eq, c))
            |> Seq.toList

        // remove the rules where there is nothing more to solve
        let remainingRules =
            simplifiedRules - (newBindings |> Seq.map fst |> Set.ofSeq)

        // add the newly discovered bindings to the map of bindings
        let allBindings =
            newBindings
            |> Seq.map snd
            |> Seq.fold (fun d (id, value) -> Map.add id value d) problem.Bindings
    
        {
            Rules = remainingRules
            Bindings = allBindings
        }

    let rec solve problem =
        let newProblem = step problem
        if newProblem = problem
            then newProblem
            else solve newProblem
