namespace FsSolver.Rules

open FsSolver

module Concretizer =
    let rec private addScopeToVariables scope expression =
        match expression with
        | Expression.Var v -> Expression.Var(Scoped(scope.Name, v))
        | Expression.BinaryNode(op, n1, n2) -> Expression.BinaryNode(op, addScopeToVariables scope n1, addScopeToVariables scope n2)
        | _ -> expression

    let concretize scope node =
        let rec concretize' doAddSCope scope node =
            let expression =
                match node with
                | Sum e ->
                    scope.Children
                    |> Seq.map (fun s -> concretize' true s e)
                    |> Seq.reduce (+)
                | Min e ->
                    scope.Children
                    |> Seq.map (fun s -> concretize' true s e)
                    |> Seq.reduce (fun x y -> Expression.BinaryNode(IfLowerThan, x, y))
                | BinaryNode(op, n1, n2) -> Expression.BinaryNode(op, concretize' false scope n1, concretize' false scope n2)
                | Const c -> Expression.Const c
                | Var v -> LocalVar v
            if doAddSCope
            then addScopeToVariables scope expression
            else expression
        concretize' true scope node
    
    let rec concretizeRule scope rule =
        seq {
            match rule with
            | Equality(n1, n2) ->
                yield concretize scope n1, concretize scope n2
            | ForAllChildren childRule ->
                for childScope in scope.Children do
                let concreteChildRule = concretizeRule childScope childRule
                yield! concreteChildRule |> Seq.map (fun (n1, n2) -> addScopeToVariables scope n1, addScopeToVariables scope n2)
        }
