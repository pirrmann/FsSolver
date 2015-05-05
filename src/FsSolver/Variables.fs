namespace FsSolver

type Variable =
    | Local of string
    | Scoped of string * Variable with
    override x.ToString() = match x with
                            | Local name -> name
                            | Scoped(scope, v) -> sprintf "%s.%s" scope (v.ToString())
