module FIDLGenerator

open AST
open Unparsing
open System.Text

let gPrimitiveType pt =
    match pt with
    | Unit -> "unit"
    | Byte -> "byte"
    | Integer -> "int"
    | Float -> "float"
    | Double -> "double"
    | GUID -> "guid"
    | String -> "string"

let gTypeRef tr =
    match tr with
    | Identifier id -> id
    | QualifiedIdentifier id -> QualifiedIdentifier.stringize id

let rec gCollectionType ct =
    match ct with
    | List itemType ->
        sprintf "list of %s" (gFIDLType itemType)
    | Map (keyType,valueType) ->
        sprintf "map of %s to %s" (gPrimitiveType keyType) (gFIDLType valueType)

and gFIDLType fidlType =
    match fidlType with
    | PrimitiveType pt -> gPrimitiveType pt
    | CollectionType ct -> gCollectionType ct
    | UnresolvedTypeRef tr -> gTypeRef tr

let gFieldTypeDecl fieldTypeDecl =
    Concat [fst fieldTypeDecl; ": "; snd fieldTypeDecl |> gFIDLType]

let gCaseDecl case =
    match snd case with
    | None -> Concat [fst case]
    | Some fidlType -> Concat [fst case; ": "; gFIDLType fidlType]

let scopeWithNewlines layout =
    Group [
        Scope [
            scopedNewline
            layout
        ]
        scopedNewline
    ]

let gRecordType (rt:Record) =
    let fields = Seq.map gFieldTypeDecl rt.Fields
    Group [
        Concat ["record "; QualifiedIdentifier.name rt.Identifier; " "]
        sepBy scopedNewline fields
        |> scopeWithNewlines
        |> inCurlyBraces
        
    ]

let gChoiceType (rt:Choice) =
    let choiceCaseSep = Group [scopedNewline; Concat ["| "]]
    let cases =
        Seq.map gCaseDecl rt.Cases
    Group [
        Concat ["choice "; QualifiedIdentifier.name rt.Identifier; " ="]
        Scope [
            choiceCaseSep
            sepBy choiceCaseSep cases
        ]
    ]

let gFunctionType (ft:Function) =
    let paramsList =
        if ft.Parameters.Length < 3
        then
            sepBy (Concat [", "]) (Seq.map gFieldTypeDecl ft.Parameters)
        else
            let newLineParamsSep =
                Group [
                    Concat [","]
                    scopedNewline
                ]
            sepBy newLineParamsSep (Seq.map gFieldTypeDecl ft.Parameters)
            |> scopeWithNewlines
    Group [
        Concat ["function "; QualifiedIdentifier.name ft.Identifier; " "]
        inBraces (paramsList)
        Concat [" -> "; gFIDLType ft.ReturnType]
    ]

let prependNewline layout =
    Group [
        scopedNewline
        layout
    ]

let rec generateFIDLLayout ast =
    match ast with
    | FIDLNamespace ns -> generateNamespace ns
    | TypeDecl fidlType ->
        match fidlType with
        | RecordType recordType -> gRecordType recordType
        | ChoiceType choiceType -> gChoiceType choiceType
        | FunctionType functionType -> gFunctionType functionType

and generateNamespace ns =
    let children =
        Seq.map (generateFIDLLayout >> prependNewline) ns.Children
    Group [
        Concat ["namespace "; QualifiedIdentifier.name ns.Identifier; " "]
        sepBy scopedNewline children
        |> scopeWithNewlines
        |> inCurlyBraces
    ]