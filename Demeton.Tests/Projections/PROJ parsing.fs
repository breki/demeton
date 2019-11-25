module Tests.Projections.``PROJ parsing``

open FParsec

open Xunit
open Swensen.Unquote
open TestHelp

// https://proj.org/usage/projections.html
// https://github.com/OSGeo/PROJ/blob/master/schemas/v0.2/projjson.schema.json
// https://proj.org/operations/projections/index.html#projections

/// <summary>
/// A PROJ parameter value.
/// </summary>
type ParameterValue =
    /// <summary>
    /// Value of a PROJ string parameter.
    /// </summary>
    | StringValue of string
    /// <summary>
    /// Value of a PROJ numeric parameter.
    /// </summary>
    | NumericValue of float
    
/// <summary>
/// A PROJ parameter.
/// </summary>
type Parameter = { Name: string; Value: ParameterValue }

type MercatorParameters = unit

type Projection =
    | Mercator of MercatorParameters

type ParsedProjection =
    { Projection: Projection; IgnoredParameters: Parameter list }

type ProjectionParsingError =
    | SpecParsingError of TextParsers.ParsingError
    | ProjectionNotSpecified
    | UnsupportedProjection of string
    
type ProjectionParsingResult = Result<ParsedProjection, ProjectionParsingError>

/// <summary>
/// A PROJ specification string.
/// </summary>
type ProjSpec = string

[<Literal>]
let ParProj = "proj"

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

/// <summary>
/// Parses PROJ specification into a list of PROJ parameters.
/// </summary>
let parseProjSpecParameters (projSpec: ProjSpec)
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

let parseProjSpecProjection (projSpec: ProjSpec): ProjectionParsingResult =
    let tryGetProjectionName parameters =
        parameters
        |> List.tryFind (fun p -> p.Name = ParProj)
        |> Option.map (fun p -> p.Value)
        |> function
        | Some (StringValue strValue) -> Some strValue
        | _ -> None
    
    let removeProjParameter parameters =
        parameters
        |> List.filter (fun p -> p.Name <> ParProj)
    
    match parseProjSpecParameters projSpec with
    | Result.Ok parameters ->
        match tryGetProjectionName parameters with
        | Some "merc" ->
            { Projection = Mercator();
                 IgnoredParameters = removeProjParameter parameters }
            |> Result.Ok
        | Some unsupportedProjection ->
            unsupportedProjection |> UnsupportedProjection |> Result.Error
        | None -> Result.Error ProjectionNotSpecified
    | Result.Error parsingError ->
        parsingError |> SpecParsingError |> Result.Error
       
[<Fact>]
let ``Parses PROJ specification that uses Mercator``() =
    let parseResult = parseProjSpecProjection "+proj=merc +lat_ts=56.5"
    test <@ parseResult
            |> isOkValue { Projection = Mercator();
                           IgnoredParameters = [
                               { Name = "lat_ts"; Value = NumericValue 56.5 }
                           ] } @>
       
[<Fact>]
let ``Reports an error if projection name is unsupported``() =
    let parseResult = parseProjSpecProjection "+proj=tmerc +lat_ts=56.5"
    test <@ parseResult |> isErrorData (UnsupportedProjection "tmerc") @>
       
[<Fact>]
let ``Reports an error if projection was not specified``() =
    let parseResult = parseProjSpecProjection "+something=tmerc +lat_ts=56.5"
    test <@ parseResult |> isErrorData ProjectionNotSpecified @>
       
[<Fact>]
let ``Reports an error if PROJ specification has a syntax error``() =
    let parseResult = parseProjSpecProjection "+proj=tmerc lat_ts=56.5"
    test <@ parseResult
            |> isErrorData (SpecParsingError {
                  Message = "Expected: parameter indicator '+'";
                  Location = 12 }) @>
