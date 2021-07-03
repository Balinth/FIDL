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
    if optionalSplit.Length < 2
    then seq {str}
    else
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

let fidlTypes : obj[] seq =
    let samples : obj[] seq = seq {
        yield [|"guid"; GUID |> PrimitiveType|]
    }
    samples
    |> Seq.collect (fun objs ->
        whiteSpacePermutations (objs.[0] :?> string)
        |> Seq.map (fun permutation -> [|permutation; objs.[1]|])
    )


[<Theory>]
[<MemberData(nameof fidlTypes)>]
let ``Should parse valid FIDL types`` typeDecl expected=
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


let typeDeclSamples : obj[] seq =
    let samples : obj[] seq = seq {
        yield [|
            "choice+result*=*|*Ok*:*string*|*Error"
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
let ``Should parse valid user type declarations`` typeDecl expected=
    // arrange
    let sut =
        run pTypeDecl
    // act
    let result =
        sut typeDecl

    // assert
    match result with
    | Failure _ ->
        failwith (sprintf "%A" result)
    | Success (actual,_,_) ->
        Assert.Equal (actual, expected)


let parserRemainderPairs : obj[] seq =
    seq {
        yield [|pPrimitive; "guid "; " "|]
        yield [|pTypeDecl; "choice Result = | Ok | Error "; " "|]
        yield [|pTypeDecl; "record User {Name: string; ID: guid} "; " "|]
        yield [|pNamespace |>> id; "namespace EmptyNS {} "; " "|] // |>> id is required due to some type casting quirk with parsers using forwarded refs
        yield [|pFIDLType |>> id; "string "; " "|] // |>> id is required due to some type casting quirk with parsers using forwarded refs
        yield [|pFIDLType |>> id; "list of string "; " "|] // |>> id is required due to some type casting quirk with parsers using forwarded refs
        yield [|pFIDLType |>> id; "map of string to list of guid "; " "|] // |>> id is required due to some type casting quirk with parsers using forwarded refs
    }

[<Theory>]
[<MemberData(nameof parserRemainderPairs)>]
let ``Parsers should only consume exactly the required characters`` parser input expectedRemainder =
    // arrange
    let sut =
        parser
        >>. manyChars anyChar
        |> run

    // act
    let result =
        sut input

    // assert
    match result with
    | Success (remainingChars,_,_) when remainingChars = expectedRemainder -> ()
    | _ -> failwith (sprintf "%A" result)

let fidlNamespaces : obj[] seq =
    let samples : obj[] seq = seq {
        yield [|
            "namespace+Empty*{*}*"
            {
                Identifier = ["Empty"]
                Children = []
            }
        |]
        
        yield [|
            "namespace+NotEmpty*{*choice+result*=*|*Ok*:*string*|*Error*;*record+User*{*ID*:*guid*;*Name*:*STRING*}*;*namespace+Empty*{*}*}*"
            {
                Identifier = ["NotEmpty"]
                Children = [
                    ChoiceType {
                        Identifier = "result"
                        Cases = Map.ofList [
                            "Ok", Some (PrimitiveType String)
                            "Error", None
                        ]
                    } |> TypeDecl
                    RecordType {
                        Identifier = "User"
                        Fields = Map.ofList [
                            "ID", GUID |> PrimitiveType
                            "Name", String |> PrimitiveType
                        ]
                    } |> TypeDecl
                    {
                        Identifier = ["Empty"]
                        Children = []
                    } |> FIDLNamespace
                ]
            }
        |]
        
        yield [|
            "namespace+NotEmpty*{*choice+result*=*|*Ok*:*string*|*Error*\r\n*record+User*{*ID*:*guid*;*Name*:*STRING*}*\r*namespace+Empty*{*}*}*"
            {
                Identifier = ["NotEmpty"]
                Children = [
                    ChoiceType {
                        Identifier = "result"
                        Cases = Map.ofList [
                            "Ok", Some (PrimitiveType String)
                            "Error", None
                        ]
                    } |> TypeDecl
                    RecordType {
                        Identifier = "User"
                        Fields = Map.ofList [
                            "ID", GUID |> PrimitiveType
                            "Name", String |> PrimitiveType
                        ]
                    } |> TypeDecl
                    {
                        Identifier = ["Empty"]
                        Children = []
                    } |> FIDLNamespace
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
[<MemberData(nameof fidlNamespaces)>]
let ``Should parse valid FIDL namespaces`` namespaceDecl expected=
    // arrange
    let sut =
        run pNamespace
    // act
    let result =
        sut namespaceDecl

    // assert
    match result with
    | Failure _ ->
        failwith (sprintf "%A" result)
    | Success (actual,_,_) ->
        Assert.Equal (actual, expected)



let invalidNamespaces : obj[] seq =
    let samples : obj[] seq = seq {
        yield [|
            "namespace+Empty*{*"
            "namespace+NotEmpty*{*choice+result*=*|*Ok*:*string*|*Error record+User*{*ID*:*guid*;*Name*:*STRING*}*;*namespace+Empty*{*}*}*"
            //                                                         ^ missing separator
        |]
    }
    samples
    |> Seq.collect (fun objs ->
        whiteSpacePermutations (objs.[0] :?> string)
        |> Seq.map (fun permutation -> [|permutation|])
    )

[<Theory>]
[<MemberData(nameof invalidNamespaces)>]
let ``Should reject invalid FIDL namespaces`` namespaceDecl=
    // arrange
    let sut =
        run pNamespace

    // act
    let result =
        sut namespaceDecl

    // assert
    match result with
    | Failure _ -> ()
    | Success (actual,_,_) ->
        failwith (sprintf "%A" result)
