module Unparsing

open System.Text

type Layout =
    | Concat of string seq
    | Group of Layout seq
    | Scope of Layout seq
    | ScopedIndent
    | Newline

let scopedNewline = Group [Newline; ScopedIndent]

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


let testAST = Group [
    Scope [
        ScopedIndent
        Concat ["somehint"; "or other"]
        scopedNewline
        Concat ["onScopedNewline"]
        Scope [
            scopedNewline
            Concat ["on inner scoped newline"]
        ]
    ]
]


let sb = StringBuilder()
unparse sb {IndentLevel = 0} testAST
sb.ToString()

module Say =
    let hello name =
        printfn "Hello %s" name
