module FsSolver

type Operator =
    | Addition
    | Substraction
    | Product
    | Division
    override op.ToString() =
        match op with
        | Addition -> "+"
        | Substraction -> "-"
        | Product -> "*"
        | Division -> "/"
    member op.ToOperator:(decimal->decimal->decimal) =
        match op with
        | Addition -> (+)
        | Substraction -> (-)
        | Product -> (*)
        | Division -> (/)

type Expression =
    | Var of string
    | Const of decimal
    | BinaryNode of Operator * Expression * Expression
    static member (+) (x, y) =  BinaryNode(Addition, x, y)
    static member (-) (x, y) =  BinaryNode(Substraction, x, y)
    static member (*) (x, y) =  BinaryNode(Product, x, y)
    static member (/) (x, y) =  BinaryNode(Division, x, y)
    override x.ToString() =
        match x with
        | Var name -> name
        | Const c -> sprintf "%M" c
        | BinaryNode(op, e1, e2) -> sprintf "(%O %O %O)" e1 op e2

type Equality = Equality of Expression * Expression with
    static member Map f e = match e with | Equality(e1, e2) -> Equality(f e1, f e2)
    override e.ToString() = match e with | Equality(e1, e2) -> sprintf "%O = %O" e1 e2

let (===) expr1 expr2 = Equality(expr1, expr2)

let rec private replaceValues values expression =
    match expression with
    | Var(name) as v -> match Map.tryFind name values with | Some(value) -> Const(value) | None -> v
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

let rec private getVariablesNames expression = seq {
    match expression with
    | BinaryNode(_, e1, e2) ->
        yield! getVariablesNames e1
        yield! getVariablesNames e2
    | Var name -> yield name
    | _ -> () }

let hasVariable = getVariablesNames >> Seq.isEmpty >> not

let rec private isolateSingleVariable eq =
    let varSide, c =
        match eq with 
        | Equality(e, Const c)
        | Equality(Const c, e) -> e, c
        | _ -> failwith "There should be a constant on one side"

    match varSide with
    | Var name -> Some(name, c)
    | BinaryNode(op, n1, n2) ->
        let newVarSide, newExpression =
            match op, hasVariable n2 with
            | Addition, false -> n1, Const c - n2
            | Substraction, false -> n1, Const c + n2
            | Product, false -> n1, Const c / n2
            | Division, false -> n1, Const c * n2
            | Addition, true -> n2, Const c - n1
            | Substraction, true -> n2, n1 - Const c
            | Product, true -> n2, Const c / n1
            | Division, true -> failwith "The variable must be in the numerator"
        isolateSingleVariable(Equality(newVarSide, simplify newExpression))
    | _ -> None
    
let private tryIsolateVariable = function
    | Equality(e1, e2) as eq ->
        let allVariablesNames =
            seq {
                yield! getVariablesNames e1
                yield! getVariablesNames e2
            } |> Seq.toList

        if allVariablesNames.Length = 1
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
        |> Seq.fold (fun d (name, value) -> Map.add name value d) bindings
    
    (remainingRules, allBindings)
