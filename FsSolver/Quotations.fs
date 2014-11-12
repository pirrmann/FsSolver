namespace FsSolver

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection.FSharpReflectionExtensions

open FsSolver

module QuotationsConverter =
    let rec private skipParameters expr =
        match expr with
        | ExprShape.ShapeLambda(_, lambdaBody) ->
            lambdaBody |> skipParameters
        | _ -> expr

    let private new_decimal_info =
        let fsharpCore = System.Reflection.Assembly.GetAssembly(typeof<Microsoft.FSharp.Core.Option<string>>)

        let languagePrimitivesIntrinsicFunctions =
            fsharpCore.DefinedTypes
            |> Seq.find (fun fi -> fi.Name = "IntrinsicFunctions")

        languagePrimitivesIntrinsicFunctions.GetMethod("MakeDecimal",
                                        System.Reflection.BindingFlags.Static
                                        ||| System.Reflection.BindingFlags.Public)

    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    let private (|Decimal|_|) = function
        | Call (None, mi, [Int32 low; Int32 medium; Int32 high; Bool isNegative; Byte scale])
          when mi.Name = new_decimal_info.Name
               && mi.DeclaringType.FullName = new_decimal_info.DeclaringType.FullName ->
            Some (new_decimal_info.Invoke(null, [|low :> obj; medium :> obj; high :> obj; isNegative :> obj; scale :> obj|]) :?> decimal)
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
            | Decimal d -> Expression.Value(Constant d)
            | _ -> failwith "Not supported pattern"

        match ruleBody with
        | DerivedPatterns.SpecificCall <@ (=) @> (None, _, [e1; e2]) ->
            (mapExpression e1, mapExpression e2)
        | _ -> failwith "The expression should be an equality"
