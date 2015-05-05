module ``Extract Links``

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``1-variable rules yield no link`` () =

    let rule = LocalVar "x" =@= ConstValue 1M

    let links = Links.Extract rule |> Set.ofSeq

    links |> shouldEqual Set.empty

let [<Test>] ``2-variables rules yield one link`` () =

    let rule = LocalVar "x" + ScopedVar(["y"; "scope"]) =@= ConstValue 1M

    let links = Links.Extract rule |> Set.ofSeq

    links |> shouldEqual (Set.ofSeq [Link(Local "x", Scoped("scope", Local "y"))])

let [<Test>] ``3-variables rules yield 3 links`` () =

    let rule = LocalVar "x" + ScopedVar(["y"; "scope"]) =@= ConstValue 1M - LocalVar "z"

    let links = Links.Extract rule |> Set.ofSeq

    links |> shouldEqual (Set.ofSeq [Link(Local "x", Scoped("scope", Local "y"))
                                     Link(Local "z", Scoped("scope", Local "y"))
                                     Link(Local "x", Local "z")])
