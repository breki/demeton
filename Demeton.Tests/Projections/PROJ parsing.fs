module Tests.Projections.``PROJ parsing``

open FParsec

open Xunit
open Swensen.Unquote
open TestHelp

// https://proj.org/usage/projections.html
// https://github.com/OSGeo/PROJ/blob/master/schemas/v0.2/projjson.schema.json
// https://proj.org/operations/projections/index.html#projections

type ParameterValue =
    | StringValue of string
    | NumericValue of float
    
type Parameter = { Name: string; Value: ParameterValue }

//type MercatorParameters = {}
//
//type Projection =
//    | Mercator of MercatorParameters

type ProjSpec = string

let isAlphanumeric x = isAsciiLetter x || isDigit x || x = '_'

let pParName: Parser<string, unit> =
    many1SatisfyL isAlphanumeric "parameter name"

let pParStringValue: Parser<ParameterValue, unit> =
    many1SatisfyL isAlphanumeric "string value"
    |>> (fun strValue -> StringValue strValue)

let pParNumericValue: Parser<ParameterValue, unit> =
    pfloat <?> "numeric value"
    |>> (fun floatValue -> NumericValue floatValue)

let pParValue: Parser<ParameterValue, unit> =
    (pParNumericValue <|> pParStringValue) <?> "parameter value"

let pParIndicator = (pstring "+") <?> "parameter indicator '+'"

let pParAssigment = (pstring "=") <?> "parameter value assignment '='"

let pProjParameter: Parser<Parameter, unit> =
    pipe4 pParIndicator pParName pParAssigment pParValue
        (fun _ parName _ parValue -> { Name = parName; Value = parValue })

let pProj: Parser<Parameter list, unit> =
    sepBy pProjParameter spaces1 <?> "parameters"

let parseProjSpecParameters projSpec
    : Result<Parameter list, TextParsers.ParsingError> =
    match run pProj projSpec with
    | Success (steps, _, _) -> Result.Ok steps
    | Failure (_, parserError, _) -> 
        TextParsers.formatParsingFailure parserError

[<Fact>]
let ``Successfully parses PROJ specification into parameters list``() =
    let result = parseProjSpecParameters "+proj=merc +lat_ts=56.5"
    
    test <@ result |> isOkValue [
                { Name = "proj"; Value = StringValue "merc" };    
                { Name = "lat_ts"; Value = NumericValue 56.5 }    
                ]  @>

[<Fact>]
let ``Reports an error if PROJ parameter does not start with +``() =
    let result = parseProjSpecParameters "+proj=merc lat_ts=56.5"
    
    test <@ result |> isErrorData 
                { Message = "Expected: parameter indicator '+'";
                  Location = 11 } @>

[<Fact>]
let ``Reports an error if PROJ parameter name is missing``() =
    let result = parseProjSpecParameters "+=merc +lat_ts=56.5"
    
    test <@ result |> isErrorData 
                { Message = "Expected: parameter name";
                  Location = 1 } @>

[<Fact>]
let ``Reports an error if PROJ parameter does not continue with =``() =
    let result = parseProjSpecParameters "+proj +lat_ts=56.5"
    
    test <@ result |> isErrorData 
                { Message = "Expected: parameter value assignment '='";
                  Location = 5 } @>

[<Fact>]
let ``Reports an error if PROJ parameter does not have a value assigned``() =
    let result = parseProjSpecParameters "+proj= +lat_ts=56.5"
    
    test <@ result |> isErrorData 
                { Message = "Expected: parameter value";
                  Location = 6 } @>
