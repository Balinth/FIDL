module Unparsing

open System.Text

type Layout =
    | Concat of string seq
    | Group of Layout seq
    | Scope of Layout seq
    | ScopedIndent
    | Newline

type UnparserState = {
    IndentLevel: int
}

let rec unparse (stringBuilder : StringBuilder) state layout =
    match layout with
    | ScopedIndent -> stringBuilder.Append('\t', state.IndentLevel)
    | Newline -> stringBuilder.AppendLine()
    | Concat strings ->
        strings
        |> Seq.iter (fun s -> stringBuilder.Append(s) |> ignore)
        stringBuilder
    | Group layouts ->
        layouts
        |> Seq.iter (unparse stringBuilder state >> ignore)
        stringBuilder
    | Scope layouts ->
        layouts
        |> Seq.iter (unparse stringBuilder {state with IndentLevel = state.IndentLevel + 1} >> ignore)
        stringBuilder

let scopedNewline = Group [Newline; ScopedIndent]

let inBraces layout =
    Group [
        Concat ["("]
        layout
        Concat [")"]
    ]

let inSquareBraces layout =
    Group [
        Concat ["["]
        layout
        Concat ["]"]
    ]
    
let inCurlyBraces layout =
    Group [
        Concat ["{"]
        layout
        Concat ["}"]
    ]

let sepBy separator items =
    items
    |> Seq.fold (fun state item ->
        match state with
        | empty when Seq.isEmpty empty -> seq [item]
        | someItems ->
            Seq.concat [someItems; seq [separator]; seq [item]]) Seq.empty
    |> Group