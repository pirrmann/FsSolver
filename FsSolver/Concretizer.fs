namespace FsSolver.Rules

open FsSolver

module Concretizer =
    let concretize scope node =
        let rec addScopeToVariables scope node =
            match node with
            | Var name -> InnerScopeVar(name, [scope.Name])
            | InnerScopeVar(name, varScope) -> InnerScopeVar(name, scope.Name :: varScope)
            | OuterScopeVar(name, 1) -> Var name
            | OuterScopeVar(name, levelsUp) -> OuterScopeVar(name, levelsUp - 1)
            | BinaryNode(op, n1, n2) -> BinaryNode(op, addScopeToVariables scope n1, addScopeToVariables scope n2)
            | _ -> node

        let rec flatten doAddSCope scope node =
            let newNode =
                match node with
                | Sum e ->
                    scope.Children
                    |> Seq.map (fun s -> flatten true s e)
                    |> Seq.reduce (+)
                | Min e ->
                    scope.Children
                    |> Seq.map (fun s -> flatten true s e)
                    |> Seq.reduce (fun x y -> BinaryNode(IfLowerThan, x, y))
                | BinaryNode(op, n1, n2) -> BinaryNode(op, flatten false scope n1, flatten false scope n2)
                | _ -> node

            if doAddSCope
            then addScopeToVariables scope newNode
            else newNode

        let rec toExpression node =
            match node with
            | BinaryNode(op, n1, n2) -> Expression.BinaryNode(op, n1 |> toExpression, n2 |> toExpression)
            | Const c -> Expression.Value(c, Constant)
            | Var name -> LocalVar name
            | InnerScopeVar(name, scope) -> ScopedVar (scope @ [name])
            | OuterScopeVar(_, _)
            | Sum _
            | Min _ -> failwith "Error"

        node
        |> flatten true scope
        |> toExpression
    
    let rec concretizeRule scope rule =
        let rec addScopeToVariables scope expression =
            match expression with
            | Expression.Var v -> Expression.Var(Scoped(scope.Name, v))
            | Expression.BinaryNode(op, n1, n2) -> Expression.BinaryNode(op, addScopeToVariables scope n1, addScopeToVariables scope n2)
            | _ -> expression

        seq {
            match rule with
            | Equality(n1, n2) ->
                yield concretize scope n1, concretize scope n2
            | ForAllChildren childRule ->
                for childScope in scope.Children do
                let concreteChildRule = concretizeRule childScope childRule
                yield! concreteChildRule |> Seq.map (fun (n1, n2) -> addScopeToVariables scope n1, addScopeToVariables scope n2)
        }
