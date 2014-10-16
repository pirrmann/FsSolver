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

type Expression =
    | Var of Variable
    | Const of decimal
    | BinaryNode of Operator * Expression * Expression
    | Sum of Expression
    | Min of Expression
    static member (+) (x, y) =  BinaryNode(Addition, x, y)
    static member (-) (x, y) =  BinaryNode(Substraction, x, y)
    static member (*) (x, y) =  BinaryNode(Product, x, y)
    static member (/) (x, y) =  BinaryNode(Division, x, y)
    override x.ToString() =
        match x with
        | Var id -> id.ToString()
        | Const c -> sprintf "%M" c
        | BinaryNode(op, e1, e2) -> sprintf (new PrintfFormat<_,_,_,_>(op.FormatString)) e1  e2
        | Sum(e) -> sprintf "Sum(%O)" e
        | Min(e) -> sprintf "Min(%O)" e

type Equality = Equality of Expression * Expression with 
    static member Map f e = match e with | Equality(e1, e2) -> Equality(f e1, f e2)
    override e.ToString() = match e with | Equality(e1, e2) -> sprintf "%O = %O" e1 e2

type Rule =
    | Relation of Equality
    | ForAllChildren of Rule

[<AutoOpen>]
module Notations =
    let LocalVar = Local >> Var

    let ScopedVar names =
        match List.rev names with
        | [] -> failwith "Can't build a scoped variable from a empty list"
        | local :: scope ->
            scope |> List.fold (fun s name -> Scoped(name, s)) (Local local)
        |> Var

    let AreEqual = Relation << Equality
    let (=@=) expr1 expr2 = Equality(expr1, expr2)
    let (===) expr1 expr2 = AreEqual(expr1, expr2)
