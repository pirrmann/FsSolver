namespace FsSolver

type Operator =
    | Addition
    | Substraction
    | Product
    | Division
    | IfLowerThan
    member op.FormatString =
        match op with
        | Addition -> "%O + %O"
        | Substraction -> "%O - %O"
        | Product -> "%O * %O"
        | Division -> "%O / %O"
        | IfLowerThan -> "min(%O, %O)"
    member op.ToOperator:(decimal->decimal->decimal) =
        match op with
        | Addition -> (+)
        | Substraction -> (-)
        | Product -> (*)
        | Division -> (/)
        | IfLowerThan -> min

type Variable =
    | Local of string
    | Scoped of string * Variable with
    override x.ToString() = match x with
                            | Local name -> name
                            | Scoped(scope, v) -> sprintf "%s_%s" scope (v.ToString())

[<RequireQualifiedAccess>]
type Expression =
    | Var of Variable
    | Value of decimal * ValueSource
    | BinaryNode of Operator * Expression * Expression
    static member (+) (x, y) =  BinaryNode(Addition, x, y)
    static member (-) (x, y) =  BinaryNode(Substraction, x, y)
    static member (*) (x, y) =  BinaryNode(Product, x, y)
    static member (/) (x, y) =  BinaryNode(Division, x, y)
    override x.ToString() =
        match x with
        | Var id -> id.ToString()
        | Value(c, Constant) -> sprintf "%M" c
        | Value(_, Computed e) -> e.ToString()
        | BinaryNode(op, e1, e2) -> sprintf (new PrintfFormat<_,_,_,_>(op.FormatString)) e1  e2
and ValueSource =
    | Constant
    | Computed of Expression