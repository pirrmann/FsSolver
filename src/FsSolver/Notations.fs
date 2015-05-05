namespace FsSolver

[<AutoOpen>]
module Notations =
    let LocalVar = Local >> Expression.Var

    let ScopedVariable names =
        match names with
        | [] -> failwith "Can't build a scoped variable from a empty list"
        | local :: scope ->
            scope |> List.fold (fun s name -> Scoped(name, s)) (Local local)

    let ScopedVar = ScopedVariable >> Expression.Var

    let ConstValue c = Expression.Value(Constant c)
    let ComputedValue = Computed >> Expression.Value
    let ProvidedValue (d, v) = Provided(d, v, Propagate) |> Expression.Value

    let ProvidedWith value id = id, Provided(value, id, Propagate)

    let (=@=) expr1 expr2 = expr1, expr2
    let (===) expr1 expr2 = Rules.Equality(expr1, expr2)

    let inline ParentVar name = Rules.OuterScopeVar(name, 1)
    let inline (!) x = Rules.Var x
    let inline Σ xs = Rules.Sum xs
    let MinOf nodes = nodes |> Seq.reduce (fun x y -> Rules.BinaryNode(Operator.MinOf, x, y))
    let MaxOf nodes = nodes |> Seq.reduce (fun x y -> Rules.BinaryNode(Operator.MaxOf, x, y))
