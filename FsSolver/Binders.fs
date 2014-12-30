namespace FsSolver

open System.Collections.Generic
open System.Linq.Expressions

type GetterSetter = {
    Get: unit -> decimal option;
    Set: (decimal -> unit) option } with

    static member FromLambda (property:Expression<System.Func<System.Nullable<decimal>>>) =
        let fromBody (body:Expression) needConvert =
            match body with
            | :? MemberExpression as m ->
                let getter = property.Compile()
                let setter =
                    let param = Expression.Parameter(typeof<decimal>)
                    let assignedValue = if needConvert then Expression.Convert(param, typeof<System.Nullable<decimal>>) :> Expression else param :> Expression
                    let assign = Expression.Lambda<System.Action<decimal>>(Expression.Assign(m, assignedValue), param)
                    assign.Compile()
                {
                    Get = fun () ->
                            let v = getter.Invoke()
                            if v.HasValue then Some(v.Value) else None
                    Set = Some(fun v -> setter.Invoke(v))
                }
            | _ -> invalidArg "property" "This is not a direct member access"

        match property.Body with
        | :? UnaryExpression as u when u.NodeType = ExpressionType.Convert ->
            fromBody u.Operand false
        | _ -> fromBody property.Body true

    static member FromProperty (property:System.Reflection.PropertyInfo, data) =
        let getter = property.GetGetMethod()
        let setter = property.GetSetMethod()
        {
            Get = fun () ->
                let value = getter.Invoke(data, [||])
                match value with
                | :? System.Nullable<decimal> as d when d.HasValue -> Some d.Value
                | :? decimal as d -> Some d
                | _ ->  None
            Set = if setter = null then None
                  else
                    Some(fun value -> setter.Invoke(data, [| value |]) |> ignore)
        }

type Binder = Binder of variable:Variable * getterSetter:GetterSetter with
    static member Scoped(property, names) =
        let scopedVar =
            match names |> Array.toList with
            | [] -> failwith "Can't build a scoped binder from a empty array"
            | local :: scope ->
                scope |> List.fold (fun s name -> Scoped(name, s)) (Local local)
        Binder(scopedVar, GetterSetter.FromLambda property)
