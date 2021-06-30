﻿module Parsing

open System
open FParsec

open AST

// with optional whitespace between
let (.>.) a b = a .>> (spaces) .>>. b
let (>.) a b = a .>> (spaces) >>. b
let (.>) a b = a .>> (spaces) .>> b

// with mandatory whitespace between
let (.>.>.) a b = a .>> (spaces1) .>>. b
let (>.>.) a b = a .>> (spaces1) >>. b
let (.>.>) a b = a .>> (spaces1) .>> b

let pCurlyBraces p =
    pchar '{'
    >. p
    .> pchar '}'

let primitiveTokenToString token =
    match token with
    | Integer -> "int"
    | Float -> "float"
    | Double -> "double"
    | String -> "string"
    | GUID -> "guid"
    | Unit -> "unit"
    | Byte -> "byte"

let createParserForPrimitive token =
    pstringCI (primitiveTokenToString token) >>% token

let pPrimitive =
    choice [
        createParserForPrimitive Integer
        createParserForPrimitive Unit
        createParserForPrimitive Byte
        createParserForPrimitive Float
        createParserForPrimitive Double
        createParserForPrimitive GUID
        createParserForPrimitive String
    ]

let pIdentifier =
    asciiLetter <|> (pchar '_')
    .>>. manyChars (asciiLetter <|> digit <|> (pchar '_'))
    |>> (fun (c,str) -> c.ToString() + str)

let pQualifiedIdentifier =
    sepBy1 (pIdentifier .>> spaces) (pchar '.' .>> spaces)

let pFIDLType =
    let pFIDLType, pRef = createParserForwardedToRef()

    let pPrimitiveType =
        pPrimitive
        |>> PrimitiveType

    let pListType =
        pstringCI "list"
        >.>. pstringCI "of"
        >.>. pFIDLType
        |>> List
        |>> CollectionType

    let pMapType =
        pstringCI "map"
        >.>. pstringCI "of"
        >.>. pPrimitive
        .>.> (pstringCI "to")
        .>.>. pFIDLType
        |>> Map
        |>> CollectionType

    let nonNewlineSpace =
        pchar ' '
        <|> pchar '\t'

    let pTypeAssignment =
        pIdentifier
        .> (pchar ':')
        .>. pFIDLType
        .>> (many nonNewlineSpace)
    
    let pOptTypeAssignment =
        pIdentifier
        .>. opt (
                (pchar ':')
                >. pFIDLType)

    let pRecordType =
        pstringCI "record"
        >.>. pIdentifier
        .>. (pCurlyBraces (
                sepEndBy1 
                    pTypeAssignment
                    ((
                        attempt (newline >. pchar ';')
                        <|> pchar ';'
                        <|> newline) .>> spaces)
            )
        )
        |>> (fun (identifier,fields) ->
            {
                Identifier=identifier
                Fields=Map.ofList fields
            } |> RecordType
        )

    let pChoiceType =
        pstringCI "choice"
        >.>. pIdentifier
        .>. many1 (
            pchar '|'
            >. pOptTypeAssignment
            .>> spaces
        )
        |>> (fun (identifier, cases) ->
            {Identifier=identifier;Cases=Map.ofList cases}
            |> ChoiceType)

    let pUnresolvedTypeRef =
        pQualifiedIdentifier
        |>> (fun ids ->
            match ids with
            | [one] -> Identifier one
            | more -> QualifiedIdentifier more
        ) |>> UnresolvedTypeRef

    pRef := choice [
        pPrimitiveType
        pListType
        pMapType
        pRecordType
        pChoiceType
        pUnresolvedTypeRef
    ]

    pFIDLType

let pFieldDecl =
    pIdentifier
    .> (pchar ':')
    .>. (pPrimitive)

let test1 =
    run pPrimitive "guid"

let test2 =
    run pIdentifier "lala12_43kaka"
    
