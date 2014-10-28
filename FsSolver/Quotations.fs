﻿namespace FsSolver

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection.FSharpReflectionExtensions

open FsSolver

module QuotationsConverter =
    let rec private skipParameters expr =
        match expr with
        | ExprShape.ShapeLambda(_, lambdaBody) ->
            lambdaBody |> skipParameters
        | _ -> expr

    let private (|Decimal|_|) =
        let fsharpCore = System.Reflection.Assembly.GetAssembly(typeof<Microsoft.FSharp.Core.Option<string>>)

        let languagePrimitivesIntrinsicFunctions =
            fsharpCore.DefinedTypes
            |> Seq.find (fun fi -> fi.Name = "IntrinsicFunctions")

        let makeDecimalInfo =
            languagePrimitivesIntrinsicFunctions.GetMethod("MakeDecimal",
                                         System.Reflection.BindingFlags.Static
                                         ||| System.Reflection.BindingFlags.Public)

        fun (expr:Expr) ->
            match expr with
            | Patterns.Call(None, mi, valuesExpressions)
                when mi.MetadataToken = makeDecimalInfo.MetadataToken ->
                let intValues =
                    valuesExpressions
                    |> Seq.map (function
                                | Patterns.Value (v, _) -> v
                                | _ -> failwith "Something is going really bad")
                    |> Seq.toArray
                Some(makeDecimalInfo.Invoke(null, intValues) :?> decimal)
            | _ -> None

    let ToSidesOfEquality ruleAsExpression =
        let ruleBody = ruleAsExpression |> skipParameters

        let rec mapExpression expr =
            match expr with
            | DerivedPatterns.SpecificCall <@ (+) @> (None, _, [e1; e2]) ->
                Expression.BinaryNode(Addition, mapExpression e1, mapExpression e2)
            | DerivedPatterns.SpecificCall <@ (-) @> (None, _, [e1; e2]) ->
                Expression.BinaryNode(Substraction, mapExpression e1, mapExpression e2)
            | DerivedPatterns.SpecificCall <@ (*) @> (None, _, [e1; e2]) ->
                Expression.BinaryNode(Product, mapExpression e1, mapExpression e2)
            | DerivedPatterns.SpecificCall <@ (/) @> (None, _, [e1; e2]) ->
                Expression.BinaryNode(Division, mapExpression e1, mapExpression e2)
            | Patterns.Var v -> LocalVar (v.ToString())
            | Decimal d -> Expression.Value(d, Constant)
            | _ -> failwith "Not supported pattern"

        match ruleBody with
        | DerivedPatterns.SpecificCall <@ (=) @> (None, _, [e1; e2]) ->
            (mapExpression e1, mapExpression e2)
        | _ -> failwith "The expression should be an equality"