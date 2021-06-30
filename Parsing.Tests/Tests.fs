module Tests

open System
open Xunit
open FSharp.Reflection

open AST
open Parsing
open FParsec

// any * will be replaced with zero or more whitespaces
// any + will be replaced with one or more whitespaces
// whitesapce: ' ', '\t', '\r', "\r\n", '\n'
let whiteSpacePermutations (str:string) =
    let optionalSplit = str.Split('*')
    seq {
        String.Join("", optionalSplit)
        String.Join(" ", optionalSplit)
        String.Join("\t", optionalSplit)
        String.Join("\r", optionalSplit)
        String.Join("\n", optionalSplit)
        String.Join("\r\n", optionalSplit)
    }
    |> Seq.collect (fun s ->
        let mandatorySplit = s.Split('+')
        seq {
            String.Join(" ", mandatorySplit)
            String.Join("\t", mandatorySplit)
            String.Join("\r", mandatorySplit)
            String.Join("\n", mandatorySplit)
            String.Join("\r\n", mandatorySplit)
        }
    )
    

[<Fact>]
let ``Should parse all primitive types`` () =
    // arrange
    let primitives =
        FSharpType.GetUnionCases typeof<Primitive>
        |> Seq.map (fun unionCase -> FSharpValue.MakeUnion (unionCase, null))
        |> Seq.cast<Primitive>
        |> Seq.map primitiveTokenToString

    let sut = run pPrimitive

    // act
    let results =
        Seq.map sut primitives

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
    let sut = run pIdentifier

    // act
    let result =
        sut identifier

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
    let sut = run pIdentifier

    // act
    let result =
        sut identifier

    // assert
    Assert.True (
        match result with
        | Failure _ -> true
        | _ -> false
    )

let typeDeclSamples : obj[] seq =
    let samples : obj[] seq = seq {
        yield [|"guid"; GUID |> PrimitiveType|]
        yield [|
            "choice+result*|*Ok*:*string*|*Error"
            ChoiceType {
                Identifier = "result"
                Cases = Map.ofList [
                    "Ok", Some (PrimitiveType String)
                    "Error", None
                ]
            }|]
        yield [|"record+User*{*ID*:*guid*}*"; {Identifier="User";Fields=Map.ofList ["ID",GUID |> PrimitiveType]} |> RecordType|]
        yield [|"record+User*{*ID*:*guid*;*}*"; {Identifier="User";Fields=Map.ofList ["ID",GUID |> PrimitiveType]} |> RecordType|]
        yield [|
            "record+User*{*ID*:*list+of+guid*}*"
            RecordType {
                Identifier = "User"
                Fields = Map.ofList [
                    "ID",
                    CollectionType (
                        List (
                            PrimitiveType GUID))
                ]
            }
        |]
        yield [|
            "record+User*{*things*:*map+of+guid+to+int*}*"
            RecordType {
                Identifier = "User"
                Fields = Map.ofList [
                    "things",
                    CollectionType (
                        Map (
                            GUID,
                            Integer |> PrimitiveType
                        ))
                ]
            }
        |]
        yield [|
            "record+User*{*things*:*map+of+guid+to+list+of+list+of+int*}*"
            RecordType {
                Identifier = "User"
                Fields = Map.ofList [
                    "things",
                    CollectionType (
                        Map (
                            GUID,
                            CollectionType (
                                List(
                                    CollectionType (
                                        List(
                                            PrimitiveType Integer
                                        )
                                    )
                                )
                            )
                        )
                    )
                ]
            }
        |]
        yield [|
            "record+User*{*ID*:*guid*;*Name*:*STRING*}*"
            RecordType {
                Identifier = "User"
                Fields = Map.ofList [
                    "ID", GUID |> PrimitiveType
                    "Name", String |> PrimitiveType
                ]
            }
        |]
        yield [|
            "record+User*{*ID*:*guid*;*Name*:*STRING*;*things*:*map+of+guid+to+int}*"
            RecordType {
                Identifier = "User"
                Fields = Map.ofList [
                    "ID",GUID |> PrimitiveType
                    "Name", String |> PrimitiveType
                    "things", CollectionType (
                        Map (
                            GUID,
                            Integer |> PrimitiveType
                        )
                    )
                ]
            }
        |]
        yield [|
            "record+User*{*ID*:*guid*;*Name*:*someNameSpace.someType*;*things*:*map+of+int+to+user}*"
            RecordType {
                Identifier = "User"
                Fields = Map.ofList [
                    "ID",GUID |> PrimitiveType
                    "Name", ["someNameSpace"; "someType"] |> QualifiedIdentifier |> UnresolvedTypeRef
                    "things", CollectionType (
                        Map (
                            Integer,
                            "user" |> Identifier |> UnresolvedTypeRef
                        )
                    )
                ]
            }
        |]
    }
    samples
    |> Seq.collect (fun objs ->
        whiteSpacePermutations (objs.[0] :?> string)
        |> Seq.map (fun permutation -> [|permutation; objs.[1]|])
    )


[<Theory>]
[<MemberData(nameof typeDeclSamples)>]
let ``Should parse valid type declarations`` typeDecl expected=
    // arrange
    let sut =
        run pFIDLType
    // act
    let result =
        sut typeDecl

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