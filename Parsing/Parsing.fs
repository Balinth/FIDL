module Parsing

open AST
open FParsec

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

let test1 =
    run pPrimitive "guid"
    
