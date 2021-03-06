﻿namespace FsSolver

[<RequireQualifiedAccess>]
type UnaryOperator =
    | Abs
    | Sign
    member op.FormatString =
        match op with
        | Abs -> "Abs(%O)"
        | Sign -> "Sign(%O)"
    member op.ToOperator:(decimal->decimal) =
        match op with
        | Abs -> abs
        | Sign -> sign >> (fun x -> if x = 0 then 1M else decimal x)

type ConflictHandlingStrategy = | Ignore | Propagate

[<RequireQualifiedAccess>]
type Operator =
    | Addition
    | Substraction
    | Product
    | Division
    | MinOf
    | MaxOf
    member op.FormatString =
        match op with
        | Addition -> "(%O + %O)"
        | Substraction -> "(%O - %O)"
        | Product -> "%O * %O"
        | Division -> "%O / %O"
        | MinOf -> "min(%O, %O)"
        | MaxOf -> "max(%O, %O)"
    member op.ToOperator:(decimal->decimal->decimal) =
        match op with
        | Addition -> (+)
        | Substraction -> (-)
        | Product -> (*)
        | Division -> (/)
        | MinOf -> min
        | MaxOf -> max

[<RequireQualifiedAccess>]
type Expression =
    | Var of Variable
    | Value of Value
    | UnaryNode of UnaryOperator * Expression
    | BinaryNode of Operator * Expression * Expression
    static member (+) (x, y) = BinaryNode(Operator.Addition, x, y)
    static member (-) (x, y) = BinaryNode(Operator.Substraction, x, y)
    static member (*) (x, y) = BinaryNode(Operator.Product, x, y)
    static member (/) (x, y) = BinaryNode(Operator.Division, x, y)
    override x.ToString() =
        match x with
        | Var id -> id.ToString()
        | Value(Constant c) -> sprintf "%M" c
        | Value(Provided(v, id, _)) -> sprintf "[%O (%M, provided)]" id v
        | Value(Computed(v, e)) -> sprintf "[%O = (%M, computed)]" e v
        | Value(Incoherent(e, _)) -> sprintf "[%O is incoherent]" e
        | UnaryNode(op, e) -> sprintf (new PrintfFormat<_,_,_,_>(op.FormatString)) e
        | BinaryNode(op, e1, e2) -> sprintf (new PrintfFormat<_,_,_,_>(op.FormatString)) e1  e2
and Value =
    | Constant of decimal
    | Provided of decimal * Variable * ConflictHandlingStrategy
    | Computed of decimal * Expression
    | Incoherent of Expression * Incoherence with
    static member private keepOnlyVariables expr =
        match expr with
        | Expression.Var _
        | Expression.Value(Constant _)
        | Expression.Value(Provided _)
        | Expression.Value(Computed(_, Expression.Var _)) -> expr
        | Expression.Value(Computed(_, e)) -> e
        | Expression.Value(Incoherent(e, _)) -> e
        | Expression.UnaryNode(op, e) -> Expression.UnaryNode(op, Value.keepOnlyVariables e)
        | Expression.BinaryNode(op, e1, e2) -> Expression.BinaryNode(op, Value.keepOnlyVariables e1, Value.keepOnlyVariables e2)
    member x.Evaluated = match x with
                         | Constant c -> c
                         | Provided(v, _, _)
                         | Computed(v, _) -> v
                         | Incoherent _ -> invalidOp "Trying to evaluate an incoherent value"
    member x.Expression = match x with
                          | Constant _
                          | Provided _
                          | Computed(_, Expression.Var _) -> Expression.Value x
                          | Computed(_, e) -> e
                          | Incoherent(e, _) -> e
    static member (+) (x:Value, y:Value) = Computed(x.Evaluated + y.Evaluated, x.Expression + y.Expression)
    static member (-) (x:Value, y:Value) = Computed(x.Evaluated - y.Evaluated, x.Expression - y.Expression)
    static member (*) (x:Value, y:Value) = Computed(x.Evaluated * y.Evaluated, x.Expression * y.Expression)
    static member (/) (x:Value, y:Value) = Computed(x.Evaluated / y.Evaluated, x.Expression / y.Expression)
    override x.ToString() = match x with
                            | Constant c -> sprintf "%M (constant)" c
                            | Provided(p, _, _) -> sprintf "%M (provided)" p
                            | Computed(v, e) -> sprintf "%M (%O)" v (Value.keepOnlyVariables e)
                            | Incoherent(e, Conflict es) -> sprintf "?? (%O, with conflicts between %s)" (Value.keepOnlyVariables e) (System.String.Join(" and ", es |> Seq.map (fun e -> e.ToString())))
                            | Incoherent(e, Propagated) -> sprintf "?? (%O)" (Value.keepOnlyVariables e)
and Incoherence =
    | Conflict of Expression list
    | Propagated

module Expressions =
    
    let rec internal getVariablesIds expression = seq {
        match expression with
        | Expression.UnaryNode(_, e) ->
            yield! getVariablesIds e
        | Expression.BinaryNode(_, e1, e2) ->
            yield! getVariablesIds e1
            yield! getVariablesIds e2
        | Expression.Var id -> yield id
        | _ -> () }

    let rec internal getVariablesInComputedValues expression = seq {
        match expression with
        | Expression.UnaryNode(_, e) ->
            yield! getVariablesInComputedValues e
        | Expression.BinaryNode(_, e1, e2) ->
            yield! getVariablesInComputedValues e1
            yield! getVariablesInComputedValues e2
        | Expression.Value(Computed(_, e)) ->
            yield! getVariablesInComputedValues e
        | Expression.Value(Provided(_, id, _))
        | Expression.Var id -> yield id
        | _ -> () }

    let rec combinations set = seq {
        if not (Set.isEmpty set) then
            let x = set.MinimumElement
            let set' = set.Remove x
            for y in set' do yield x, y
            yield! combinations set'
    }
