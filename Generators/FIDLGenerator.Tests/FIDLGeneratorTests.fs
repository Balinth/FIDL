module Tests

open System
open Xunit
open FParsec
open Parsing
open Unparsing
open FIDLGenerator
open System.Text

let simpleRecord = """namespace TestNS {

    record Metadata {
        Version: string
        Flags: byte
    }

    record SmartPoint {
        Id: guid
        X: float
        Ys: list of float
        Zss: map of float to list of float
        MetaData: Metadata
    }

    choice Error =
        | Unauthorized
        | Exception: string
        | ComplexGeomError: map of string to map of string to list of string

    choice PointResult = Ok: SmartPoint | Error

    function ParameterlessFunc() -> int

    function Log(msg:string) -> unit

    function SimpleFunc(id:guid, point: SmartPoint) -> PointResult

    function manyParamsFunc(
        id:guid,
        log:bool,
        source:string,
        pont:SmartPoint) -> PointResult

}
"""

let expectSuccess parserResult =
    match parserResult with
    | Success (result,_,_) -> result
    | Failure (msg, error, state) -> 
        failwith msg

[<Fact>]
let ``Parsed AST should be structurally equal between original input and generated output`` () =
    // arrange
    let parser = runParserOnString pNamespace ParserState.empty "Test string"
    let inputAST = parser simpleRecord |> expectSuccess

    // act
    let generatedLayout = generateNamespace inputAST
    let generatedString = unparse (StringBuilder()) {IndentLevel=0} generatedLayout |> string
    let roundTripAST = parser generatedString |> expectSuccess
    // assert

    Assert.Equal(inputAST, roundTripAST)
