/// <summary>
/// Parsing of PROJ specification string into map projection information.
/// </summary>
module Demeton.Projections.Parsing

open FParsec

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

/// <summary>
/// Specifies a map projection used.
/// </summary>
type ProjectionParameters =
    /// <summary>
    /// Mercator projection.
    /// </summary>
    | Mercator

/// <summary>
/// Holds information about the map projection parsed from PROJ specification,
/// including any parameters that are ignored by Demeton.
/// </summary>
type ParsedProjection =
    { Projection: ProjectionParameters; IgnoredParameters: Parameter list }

/// <summary>
/// Represents error information when PROJ specification parsing has failed. 
/// </summary>
type ProjectionParsingError =
    /// <summary>
    /// PROJ specification has a syntax error.
    /// </summary>
    | SpecParsingError of TextParsers.ParsingError
    /// <summary>
    /// PROJ specification does not contain '+proj' parameter specifying the
    /// map projection. 
    /// </summary>
    | ProjectionNotSpecified
    /// <summary>
    /// The map projection specified in the PROJ specification is not supported
    /// by Demeton. 
    /// </summary>
    | UnsupportedProjection of string
    
/// <summary>
/// The result of PROJ specification parsing, indicating the map projection or
/// an error.
/// </summary>
type ProjectionParsingResult = Result<ParsedProjection, ProjectionParsingError>

/// <summary>
/// A PROJ specification string.
/// </summary>
type ProjSpec = string

[<Literal>]
let private ParProj = "proj"

let private isAny _ = true

let private isAlphanumeric x = isAsciiLetter x || isDigit x || x = '_'

let private pParName: Parser<string, unit> =
    many1SatisfyL isAlphanumeric "parameter name"

let private pParStringValue: Parser<ParameterValue, unit> =
    many1SatisfyL isAlphanumeric "string value"
    |>> (fun strValue -> StringValue strValue)

let private pParNumericValue: Parser<ParameterValue, unit> =
    pfloat <?> "numeric value"
    |>> (fun floatValue -> NumericValue floatValue)

let private pParValue: Parser<ParameterValue, unit> =
    (pParNumericValue <|> pParStringValue) <?> "parameter value"

let private pParIndicator = (pstring "+") <?> "parameter indicator '+'"

let private pParAssigment = (pstring "=") <?> "parameter value assignment '='"

let private pProjParameter: Parser<Parameter, unit> =
    pipe4 pParIndicator pParName pParAssigment pParValue
        (fun _ parName _ parValue -> { Name = parName; Value = parValue })

let private pProj: Parser<Parameter list, unit> =
    (sepBy pProjParameter spaces1 <?> "parameter indicator '+'")
    .>> (notFollowedByL
            (many1Satisfy isAny) 
            "Unexpected character, parameter indicator '+'")

/// <summary>
/// Parses PROJ specification into a list of PROJ parameters.
/// </summary>
let parseProjSpecParameters (projSpec: ProjSpec)
    : Result<Parameter list, TextParsers.ParsingError> =
    match run pProj projSpec with
    | Success (parameters, _, _) ->
        System.Diagnostics.Debugger.Break()
        Result.Ok parameters
    | Failure (_, parserError, _) -> 
        System.Diagnostics.Debugger.Break()
        TextParsers.formatParsingFailure parserError

/// <summary>
/// Parses PROJ specification looking for a map projection, returning either the
/// information about the parsed map projection or an error.
/// </summary>
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
            { Projection = Mercator;
                 IgnoredParameters = removeProjParameter parameters }
            |> Result.Ok
        | Some unsupportedProjection ->
            unsupportedProjection |> UnsupportedProjection |> Result.Error
        | None -> Result.Error ProjectionNotSpecified
    | Result.Error parsingError ->
        parsingError |> SpecParsingError |> Result.Error
