namespace FsSolver

module Solver =

    let rec private replaceValues values expression =
        match expression with
        | Expression.Var(id) as v ->
            match Map.tryFind id values with
            | Some(value) -> Expression.Const(value)
            | None -> v
        | Expression.Const(value) as c -> c
        | Expression.BinaryNode(op, e1, e2) ->
            Expression.BinaryNode(op, replaceValues values e1, replaceValues values e2)

    let rec private simplify expression =
        match expression with
        | Expression.BinaryNode(op, e1, e2) ->
            let se1, se2 = simplify e1, simplify e2
            match se1, se2 with
            | Expression.Const(c1), Expression.Const(c2) -> Expression.Const(op.ToOperator c1 c2)
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
        let varSide, c =
            match eq with 
            | e, Expression.Const c
            | Expression.Const c, e -> e, c
            | _ -> failwith "There should be a constant on one side"

        match varSide with
        | Expression.Var id -> Some(id, c)
        | Expression.BinaryNode(op, n1, n2) ->
            let newEquality =
                match n1, n2 with
                | _, Expression.Const c2 ->
                    match op with
                    | Addition -> Some(n1, Expression.Const (c - c2))
                    | Substraction -> Some(n1, Expression.Const (c + c2))
                    | Product -> if c2 <> 0M then Some(n1, Expression.Const (c / c2)) else None
                    | Division -> if c2 <> 0M then Some(n1, Expression.Const (c * c2)) else None
                    | IfLowerThan -> None
                | Expression.Const c2, _ ->
                    match op with
                    | Addition -> Some(n2, Expression.Const (c - c2))
                    | Substraction -> Some(n2, Expression.Const (c2 - c))
                    | Product -> if c2 <> 0M then Some(n2, Expression.Const (c / c2)) else None
                    | Division -> failwith "The variable must be in the numerator"
                    | IfLowerThan -> None
                | _ -> failwith "There should be a constant on one side"
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

    let step (rules, bindings) =
        // inject the bound values in variables and simplify
        let injectAndSimplify = (replaceValues bindings) >> simplify
        let simplifiedRules = rules |> Set.map (fun (x, y) -> injectAndSimplify x, injectAndSimplify y)

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
            |> Seq.fold (fun d (id, value) -> Map.add id value d) bindings
    
        (remainingRules, allBindings)

    let rec solve (rules, bindings) =
        let newRules, newBindings = step (rules, bindings)
        if (newRules, newBindings) = (rules, bindings)
            then (newRules, newBindings)
            else solve (newRules, newBindings)
