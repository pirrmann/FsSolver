﻿namespace FsSolver.Rules

open FsSolver

type RuleNode =
    | Var of string
    | InnerScopeVar of string * (string list)
    | OuterScopeVar of string * int
    | Const of decimal
    | BinaryNode of Operator * RuleNode * RuleNode
    | Sum of RuleNode
    | Min of RuleNode
    static member (+) (x, y) = BinaryNode(Addition, x, y)
    static member (-) (x, y) = BinaryNode(Substraction, x, y)
    static member (*) (x, y) = BinaryNode(Product, x, y)
    static member (/) (x, y) = BinaryNode(Division, x, y)
    override x.ToString() =
        match x with
        | Var name -> name
        | InnerScopeVar(name, scope) -> sprintf "%s_%s" (System.String.Join("_", scope)) name
        | OuterScopeVar(name, levelsUp) -> new System.String('^', levelsUp) + name
        | Const c -> sprintf "%M" c
        | BinaryNode(op, e1, e2) -> sprintf (new PrintfFormat<_,_,_,_>(op.FormatString)) e1  e2
        | Sum(e) -> sprintf "Sum(%O)" e
        | Min(e) -> sprintf "Min(%O)" e

type Rule =
    | Equality of RuleNode * RuleNode
    | ForAllChildren of Rule

type Scope = { Name:string; Children: Scope list }
    with static member Named name = { Name = name; Children = []}