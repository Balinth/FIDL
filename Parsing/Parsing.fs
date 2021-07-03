module Parsing

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

#if DEBUG
let breakPoint (parser : Parser<_,_>) stream =
    parser stream

let spaces = breakPoint spaces

#endif

let pCurlyBraces p =
    pchar '{'
    >. p
    .> pchar '}'

let pNonNewlineSpace =
    pchar ' '
    <|> pchar '\t'

let pSemicolonOrNewlineSep =
    attempt (newline >. pchar ';')
    <|> pchar ';'
    <|> newline

let semicolonOrNewline sepByVariant p =
    sepByVariant (p .>> (many pNonNewlineSpace)) (pSemicolonOrNewlineSep .>> spaces)

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
        pUnresolvedTypeRef
    ]

    pFIDLType

let pTypeDecl =
    
    let pTypeAssignment =
        pIdentifier
        .> (pchar ':')
        .>. pFIDLType
        .>> (many pNonNewlineSpace)
    
    let pOptTypeAssignment =
        pIdentifier
        .>>. opt (
                spaces
                >>. (pchar ':')
                >. pFIDLType
                |> attempt
        )

    let pRecordType =
        pstringCI "record"
        >.>. pIdentifier
        .>. (pCurlyBraces (
                semicolonOrNewline sepEndBy1 pTypeAssignment
            )
        )
        |>> (fun (identifier,fields) ->
            {
                Identifier=identifier
                Fields=Map.ofList fields
            } |> RecordType
        )

    let pChoiceCase = pOptTypeAssignment

    let pChoiceDelim =
        (spaces
        >>. pchar '|'
        .>> spaces)
        |> attempt
        

    let pChoiceType =
        pstringCI "choice"
        >.>. pIdentifier
        .> pchar '='
        .> pchar '|'
        .>. sepBy1 pChoiceCase pChoiceDelim
        |>> (fun (identifier, cases) ->
            {Identifier=identifier;Cases=Map.ofList cases}
            |> ChoiceType)

    choice [
        pRecordType
        pChoiceType
    ]

let pNamespace : CharStream<unit> -> Reply<FIDLNamespace> =
    let pNamespace, pRef = createParserForwardedToRef()

    let contentsParser =
        pNamespace |>> FIDLNamespace
        <|> (pTypeDecl |>> TypeDecl)

    pRef :=
        pstringCI "namespace"
        >.>. pQualifiedIdentifier
        .>. (pCurlyBraces (semicolonOrNewline sepEndBy (breakPoint contentsParser)))
        |>> (fun (identifier,contents) -> {Identifier = identifier; Children = contents})

    pNamespace

let test1 =
    run pPrimitive "guid"

let test2 =
    run pIdentifier "lala12_43kaka"

let test3 =
    run pNamespace "namespace lala {}"
    
