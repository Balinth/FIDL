module Tests

open System
open Xunit
open FSharp.Reflection

open AST
open Parsing
open FParsec

[<Fact>]
let ``Should parse all primitive types`` () =
    // arrange
    let primitives =
        FSharpType.GetUnionCases typeof<Primitive>
        |> Seq.map (fun unionCase -> FSharpValue.MakeUnion (unionCase, null))
        |> Seq.cast<Primitive>
        |> Seq.map primitiveTokenToString

    // act
    let results =
        Seq.map (run pPrimitive) primitives

    // assert
    Assert.All(
        results,
        fun result ->
            Assert.True (
                match result with
                | Success _ -> true
                | _ -> false))
