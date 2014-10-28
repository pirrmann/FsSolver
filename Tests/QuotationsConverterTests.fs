module Quotations

open NUnit.Framework
open FsUnit

open FsSolver

let Const c = Expression.Value(c, Constant)

let [<Test>] ``Basic equality is converted`` () =
    let rule = <@@ fun x -> x = 1M @@>
    let converted = rule |> QuotationsConverter.ToSidesOfEquality
    converted |> should equal (LocalVar "x", Const 1M)

let [<Test>] ``Simple formula is converted`` () =
    let rule = <@@ fun x y -> x = 1M + y @@>
    let converted = rule |> QuotationsConverter.ToSidesOfEquality
    converted |> should equal (LocalVar "x", (Const 1M + LocalVar "y"))

let [<Test>] ``A more complex formula is also converted`` () =
    let rule = <@@ fun net gross execFees -> net = gross * (1M + execFees / 10000M) @@>
    let converted = rule |> QuotationsConverter.ToSidesOfEquality
    converted |> should equal (LocalVar("net"), LocalVar("gross") * (Const(1M) + LocalVar("execFees") / Const(10000M)))
