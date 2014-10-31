namespace FsSolver

type Operator =
    | Addition
    | Substraction
    | Product
    | Division
    | MinOf
    member op.FormatString =
        match op with
        | Addition -> "(%O + %O)"
        | Substraction -> "(%O - %O)"
        | Product -> "%O * %O"
        | Division -> "%O / %O"
        | MinOf -> "min(%O, %O)"
    member op.ToOperator:(decimal->decimal->decimal) =
        match op with
        | Addition -> (+)
        | Substraction -> (-)
        | Product -> (*)
        | Division -> (/)
        | MinOf -> min

type Variable =
    | Local of string
    | Scoped of string * Variable with
    override x.ToString() = match x with
                            | Local name -> name
                            | Scoped(scope, v) -> sprintf "%s_%s" scope (v.ToString())

[<RequireQualifiedAccess>]
type Expression =
    | Var of Variable
    | Value of Value
    | BinaryNode of Operator * Expression * Expression
    static member (+) (x, y) = BinaryNode(Addition, x, y)
    static member (-) (x, y) = BinaryNode(Substraction, x, y)
    static member (*) (x, y) = BinaryNode(Product, x, y)
    static member (/) (x, y) = BinaryNode(Division, x, y)
    override x.ToString() =
        match x with
        | Var id -> id.ToString()
        | Value(Constant c) -> sprintf "%M" c
        | Value(Computed(v, e)) -> sprintf "[%O = %M]" e v
        | BinaryNode(op, e1, e2) -> sprintf (new PrintfFormat<_,_,_,_>(op.FormatString)) e1  e2
and Value =
    | Constant of decimal
    | Computed of decimal * Expression with
    static member private keepOnlyVariables expr =
        match expr with
        | Expression.Var _
        | Expression.Value(Constant _)
        | Expression.Value(Computed(_, Expression.Var _)) -> expr
        | Expression.Value(Computed(_, e)) -> e
        | Expression.BinaryNode(op, e1, e2) -> Expression.BinaryNode(op, Value.keepOnlyVariables e1, Value.keepOnlyVariables e2)
    member x.Evaluated = match x with | Constant c -> c | Computed(v, _) -> v
    member x.Expression = match x with
                          | Constant _
                          | Computed(_, Expression.Var _) -> Expression.Value x
                          | Computed(_, e) -> e
    static member (+) (x:Value, y:Value) = Computed(x.Evaluated + y.Evaluated, x.Expression + y.Expression)
    static member (-) (x:Value, y:Value) = Computed(x.Evaluated - y.Evaluated, x.Expression - y.Expression)
    static member (*) (x:Value, y:Value) = Computed(x.Evaluated * y.Evaluated, x.Expression * y.Expression)
    static member (/) (x:Value, y:Value) = Computed(x.Evaluated / y.Evaluated, x.Expression / y.Expression)
    override x.ToString() = match x with
                            | Constant c -> sprintf "%M (constant)" c
                            | Computed(v, e) -> sprintf "%M (%O)" v (Value.keepOnlyVariables e)