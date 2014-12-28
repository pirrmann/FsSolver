namespace FsSolver.Rules

open FsSolver

type RuleNode =
    | Var of string
    | InnerScopeVar of string * (string list)
    | OuterScopeVar of string * int
    | Const of decimal
    | UnaryNode of UnaryOperator * RuleNode
    | BinaryNode of Operator * RuleNode * RuleNode
    | Sum of RuleNode
    | Min of RuleNode
    | Max of RuleNode
    | First of RuleNode
    | Last of RuleNode
    static member (+) (x, y) = BinaryNode(Addition, x, y)
    static member (-) (x, y) = BinaryNode(Substraction, x, y)
    static member (*) (x, y) = BinaryNode(Product, x, y)
    static member (/) (x, y) = BinaryNode(Division, x, y)
    override x.ToString() =
        match x with
        | Var name -> name
        | InnerScopeVar(name, scope) -> sprintf "%s.%s" (System.String.Join(".", scope)) name
        | OuterScopeVar(name, levelsUp) -> new System.String('^', levelsUp) + name
        | Const c -> sprintf "%M" c
        | UnaryNode(op, e) -> sprintf (new PrintfFormat<_,_,_,_>(op.FormatString)) e
        | BinaryNode(op, e1, e2) -> sprintf (new PrintfFormat<_,_,_,_>(op.FormatString)) e1  e2
        | Sum(e) -> sprintf "Sum(%O)" e
        | Min(e) -> sprintf "Min(%O)" e
        | Max(e) -> sprintf "Max(%O)" e
        | First(e) -> sprintf "First(%O)" e
        | Last(e) -> sprintf "Last(%O)" e

type Rule =
    | Equality of RuleNode * RuleNode
    | ForAllChildren of Rule

type Scope = { Name:string; Children: Scope list } with
    static member Named name = { Name = name; Children = []}
    static member Create(name, children) = {
        Name = name;
        Children = children |> Array.toList }

[<AutoOpen>]
module Shortcuts =
    let Abs node = UnaryNode(AbsoluteValue, node)
