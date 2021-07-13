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

// use this for debug purposes like so:
//let spaces = breakPoint spaces

#endif

type ParserState = {
    Types : Set<QualifiedIdentifier>
    Scope : Identifier list list
}

module ParserState =
    let empty = {
        Types = Set.empty<QualifiedIdentifier>
        Scope = []
    }

    let ensureUnique state qualifiedIdentifier =
        if state.Types.Contains(qualifiedIdentifier)
        then false
        else true

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
    
    let addTypeParserState (p: Parser<Identifier, ParserState>) : Parser<QualifiedIdentifier,ParserState> =
        p
        .>>. getUserState
        >>= (fun (identifier, state) ->
            let qualifiedIdentifier =
                identifier :: 
                List.concat state.Scope
            match state.Types.Contains qualifiedIdentifier with
            | false ->
                preturn qualifiedIdentifier
                .>> setUserState {
                    state with Types = Set.add qualifiedIdentifier state.Types
                }
            | true ->
                QualifiedIdentifier.stringize qualifiedIdentifier
                |> sprintf "The qualified typename '%s' is already taken"
                |> fail
        )

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
        |> addTypeParserState
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

let addScopeParserState (p: Parser<QualifiedIdentifier, ParserState>) : Parser<QualifiedIdentifier,ParserState> =
    p
    .>>. getUserState
    >>= (fun (parsedNamespace, state) ->
        let newScope = 
            parsedNamespace :: state.Scope
        preturn parsedNamespace
        .>> setUserState {
            state with Scope = newScope
        }
    )

let pNamespace : Parser<FIDLNamespace, ParserState> =
    
    let pushScope p =
        p
        .>>. getUserState
        >>= (fun (scopeStep,state) ->
            let newScope = scopeStep :: state.Scope
            preturn scopeStep
            .>> setUserState {
                state with Scope = newScope
            }
        )
        
    let popScope p =
        p
        .>> updateUserState (fun state ->
            match state.Scope with
            | [] ->
                failwith "We should have never reached this! the parser tried to pop more scopes than were pushed."
            | head::tail ->
                {state with Scope = tail}
        )

    
    let pNamespace, pRef = createParserForwardedToRef()

    let contentsParser =
        pNamespace |>> FIDLNamespace
        <|> (pTypeDecl |>> TypeDecl)

    pRef :=
        pstringCI "namespace"
        >.>. pQualifiedIdentifier
        |> pushScope
        //|> addScopeParserState
        .>. (pCurlyBraces (contentsParser .>> spaces |> many))
        .>>. getUserState
        |>> (fun ((identifier,contents), state) -> {Identifier = List.concat state.Scope; Children = contents})
        |> popScope

    pNamespace
