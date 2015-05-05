namespace FsSolver

open System.Collections.Generic
open System.Linq.Expressions

type DecimalGetterSetter = {
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

type SolverValueGetterSetter =  {
    Get: unit -> SolverValue option;
    Set: (SolverValue -> unit) option } with

    static member FromProperty (property:System.Reflection.PropertyInfo, data) =
        let getter = property.GetGetMethod()
        let setter = property.GetSetMethod()
        {
            Get = fun () ->
                let value = getter.Invoke(data, [||]) :?> SolverValue
                if obj.ReferenceEquals(value, null) then None
                else Some value
            Set = if setter = null then None
                  else
                    Some(fun value -> setter.Invoke(data, [| value |]) |> ignore)
        }

type GetterSetter = 
    | DecimalGetterSetter of DecimalGetterSetter
    | SolverValueGetterSetter of SolverValueGetterSetter with
    member this.Get() =
        match this with
        | SolverValueGetterSetter gs ->
            gs.Get()
        | DecimalGetterSetter gs ->
            // lift the decimal to a solver value
            gs.Get() |> Option.map (SolverValue.op_Implicit)
    member this.Set =
        match this with
        | SolverValueGetterSetter gs ->
            gs.Set
        | DecimalGetterSetter gs ->
            gs.Set |> Option.map (fun setter -> function
                                    | SolverValue.Provided(v, _)
                                    | SolverValue.Computed(v, _) -> setter(v)
                                    | _ -> ())

type Binder = Binder of variable:Variable * getterSetter:GetterSetter
