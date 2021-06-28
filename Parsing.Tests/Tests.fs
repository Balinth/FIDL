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

[<Theory>]
[<InlineData("id")>]
[<InlineData("_id")>]
[<InlineData("id2")>]
[<InlineData("Dd2")>]
[<InlineData("_id2")>]
[<InlineData("__aafdas_id__d__2432_da423")>]
[<InlineData("__Aafdas_id__D__2432_DA423")>]
let ``Should correctly parse valid identifiers`` identifier =
    // arrange

    // act
    let result =
        run pIdentifier identifier

    // assert
    Assert.True (
        match result with
        | Success (parsedIdentifier,_,_) when parsedIdentifier = identifier -> true
        | _ -> false
    )


[<Theory>]
[<InlineData("12id")>]
[<InlineData("")>]
[<InlineData(" ")>]
[<InlineData(":ds")>]
[<InlineData("12id")>]
[<InlineData("12id")>]
[<InlineData(" _id")>]
[<InlineData("ûid2")>]
[<InlineData("\Dd2")>]
[<InlineData("*_id2")>]
[<InlineData("-__aafdas_id__d__2432_da423")>]
[<InlineData("+__Aafdas_id__D__2432_DA423")>]
let ``Should reject invalid identifiers`` identifier =
    // arrange

    // act
    let result =
        run pIdentifier identifier

    // assert
    Assert.True (
        match result with
        | Failure _ -> true
        | _ -> false
    )

let typeDeclSamples : obj[] seq =
    seq {
        yield [|"guid"; GUID |> PrimitiveType|]
        yield [|"record User {ID:guid}"; {Identifier="User";Fields=Map.ofList ["ID",GUID |> PrimitiveType]} |> RecordType|]
        yield [|"record User {ID:list of guid}"; {Identifier="User";Fields=Map.ofList ["ID",GUID |> PrimitiveType |> List |> CollectionType]} |> RecordType|]
    }

[<Theory>]
[<MemberData(nameof typeDeclSamples)>]
let ``Should parse valid type declarations`` typeDecl expected=
    // arrange

    // act
    let result =
        run pFIDLType typeDecl

    // assert
    match result with
    | Failure _ ->
        failwith (sprintf "%A" result)
    | Success (actual,_,_) ->
        Assert.Equal (actual, expected)
    //Assert.True(
    //    match result with
    //    | Success (result,_,pos) when pos.Index = (int64 typeDecl.Length) -> true
    //    | _ -> false
    //)