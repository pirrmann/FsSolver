module Quotations

open NUnit.Framework
open FsUnit

open FsSolver

let ConstValue c = Expression.Value(Constant c)

let [<Test>] ``Basic equality is converted`` () =
    let rule = <@@ fun x -> x = 1M @@>
    let converted = rule |> QuotationsConverter.ToSidesOfEquality
    converted |> should equal (LocalVar "x", ConstValue 1M)

let [<Test>] ``Simple formula is converted`` () =
    let rule = <@@ fun x y -> x = 1M + y @@>
    let converted = rule |> QuotationsConverter.ToSidesOfEquality
    converted |> should equal (LocalVar "x", (ConstValue 1M + LocalVar "y"))

let [<Test>] ``A more complex formula is also converted`` () =
    let rule = <@@ fun net gross execFees -> net = gross * (1M + execFees / 10000M) @@>
    let converted = rule |> QuotationsConverter.ToSidesOfEquality
    converted |> should equal (LocalVar("net"), LocalVar("gross") * (ConstValue 1M + LocalVar("execFees") / ConstValue 10000M))
