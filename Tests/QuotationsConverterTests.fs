module QuotationsConverterTests

open NUnit.Framework
open FsUnit

open FsSolver

let [<Test>] ``Basic equality is converted`` () =
    let rule = <@@ fun x -> x = 1M @@>
    let converted = rule |> QuotationsConverter.ToEquality
    converted |> should equal (Var "x" === Const 1M)

let [<Test>] ``Simple formula is converted`` () =
    let rule = <@@ fun x y -> x = 1M + y @@>
    let converted = rule |> QuotationsConverter.ToEquality
    converted |> should equal (Var "x" === (Const 1M + Var "y"))

let [<Test>] ``A more complex formula is also converted`` () =
    let rule = <@@ fun net gross execFees -> net = gross * (1M + execFees / 10000M) @@>
    let converted = rule |> QuotationsConverter.ToEquality
    converted |> should equal (Var("net") === Var("gross") * (Const(1M) + Var("execFees") / Const(10000M)))
