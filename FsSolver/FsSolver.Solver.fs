namespace FsSolver

module Solver =
    let rec private replaceValues values expression =
        match expression with
        | Var(id) as v -> match Map.tryFind id values with | Some(value) -> Const(value) | None -> v
        | Const(value) as c -> c
        | BinaryNode(op, e1, e2) -> BinaryNode(op, replaceValues values e1, replaceValues values e2)

    let rec private simplify expression =
        match expression with
        | BinaryNode(op, e1, e2) ->
            let se1, se2 = simplify e1, simplify e2
            match se1, se2 with
            | Const(c1), Const(c2) -> Const(op.ToOperator c1 c2)
            | _ -> BinaryNode(op, se1, se2)
        | _ -> expression

    let rec private getVariablesIds expression = seq {
        match expression with
        | BinaryNode(_, e1, e2) ->
            yield! getVariablesIds e1
            yield! getVariablesIds e2
        | Var id -> yield id
        | _ -> () }

    let hasVariable = getVariablesIds >> Seq.isEmpty >> not

    let rec private isolateSingleVariable eq =
        let varSide, c =
            match eq with 
            | Equality(e, Const c)
            | Equality(Const c, e) -> e, c
            | _ -> failwith "There should be a constant on one side"

        match varSide with
        | Var id -> Some(id, c)
        | BinaryNode(op, n1, n2) ->
            let newEquality =
                match n1, n2 with
                | _, Const c2 ->
                    match op with
                    | Addition -> Some(n1, Const (c - c2))
                    | Substraction -> Some(n1, Const (c + c2))
                    | Product -> if c2 <> 0M then Some(n1, Const (c / c2)) else None
                    | Division -> if c2 <> 0M then Some(n1, Const (c * c2)) else None
                | Const c2, _ ->
                    match op with
                    | Addition -> Some(n2, Const (c - c2))
                    | Substraction -> Some(n2, Const (c2 - c))
                    | Product -> if c2 <> 0M then Some(n2, Const (c / c2)) else None
                    | Division -> failwith "The variable must be in the numerator"
                | _ -> failwith "There should be a constant on one side"
            newEquality |> Option.map Equality |> Option.bind isolateSingleVariable
        | _ -> None
    
    let private tryIsolateVariable = function
        | Equality(e1, e2) as eq ->
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
        let simplifiedRules = rules |> Set.map (Equality.Map ((replaceValues bindings) >> simplify))

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