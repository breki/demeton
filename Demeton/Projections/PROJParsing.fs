/// <summary>
/// Parsing of PROJ specification string into map projection information.
/// </summary>
module Demeton.Projections.PROJParsing

open Demeton.Projections.Common
open FParsec

// https://proj.org/usage/projections.html
// https://github.com/OSGeo/PROJ/blob/master/schemas/v0.2/projjson.schema.json
// https://proj.org/operations/projections/index.html#projections


/// <summary>
/// Specifies a map projection used.
/// </summary>
type PROJParameters =
    /// <summary>
    /// Lambert Conformal Conic projection.
    /// </summary>
    | LambertConformalConic of LambertConformalConic.Parameters
    /// <summary>
    /// Mercator projection.
    /// </summary>
    | Mercator

/// <summary>
/// Holds information about the map projection parsed from PROJ specification,
/// including any parameters that are ignored by Demeton.
/// </summary>
type PROJProjection =
    { Projection: PROJParameters; IgnoredParameters: PROJParameter list }

/// <summary>
/// Represents error information when PROJ specification parsing has failed. 
/// </summary>
type PROJError =
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
    /// One of map projection parameters in the PROJ specification has an invalid value.
    /// </summary>
    | InvalidProjectionParameters of string
    
/// <summary>
/// The result of PROJ specification parsing, indicating the map projection or
/// an error.
/// </summary>
type PROJParsingResult = Result<PROJProjection, PROJError>

/// <summary>
/// A PROJ specification string.
/// </summary>
type PROJSpec = string

[<Literal>]
let private ParProj = "proj"

let private isAny _ = true

let private isAlphanumeric x = isAsciiLetter x || isDigit x || x = '_'

let private pParName: Parser<string, unit> =
    many1SatisfyL isAlphanumeric "parameter name"

let private pParStringValue: Parser<PROJParameterValue, unit> =
    many1SatisfyL isAlphanumeric "string value"
    |>> StringValue

let private pParNumericValue: Parser<PROJParameterValue, unit> =
    pfloat <?> "numeric value"
    |>> NumericValue

let private pParValue: Parser<PROJParameterValue, unit> =
    (pParNumericValue <|> pParStringValue) <?> "parameter value"

let private pParIndicator = (pstring "+") <?> "parameter indicator '+'"

let private pParAssigmentSymbol =
    (pstring "=") <?> "parameter value assignment '='"

let private pPar = pParIndicator >>. pParName

let private pParValueAssignment =
    opt (pParAssigmentSymbol >>. pParValue)

let private pProjParameter =
    (pPar .>>.? pParValueAssignment)
    |>> (fun (parName, parValue) -> { Name = parName; Value = parValue })

let private pProj: Parser<PROJParameter list, unit> =
    (sepBy pProjParameter spaces1 <?> "parameter indicator '+'")
    .>> (notFollowedByL
            (many1Satisfy isAny) 
            "Unexpected character, parameter indicator '+'")

/// <summary>
/// Parses PROJ specification into a list of PROJ parameters.
/// </summary>
let parseProjSpecParameters (projSpec: PROJSpec)
    : Result<PROJParameter list, TextParsers.ParsingError> =
    match run pProj projSpec with
    | Success (parameters, _, _) -> Result.Ok parameters
    | Failure (_, parserError, _) -> 
        TextParsers.formatParsingFailure parserError

/// <summary>
/// Parses PROJ specification looking for a map projection, returning either the
/// information about the parsed map projection or an error.
/// </summary>
let parseProjSpecProjection (projSpec: PROJSpec): PROJParsingResult =
    let tryGetProjectionName parameters =
        parameters
        |> List.tryFind (fun p -> p.Name = ParProj)
        |> Option.map (fun p -> p.Value)
        |> function
        | Some (Some (StringValue strValue)) -> Some strValue
        | _ -> None
    
    let removeProjParameter parameters =
        parameters
        |> List.filter (fun p -> p.Name <> ParProj)
    
    match parseProjSpecParameters projSpec with
    | Result.Ok parameters ->
        match tryGetProjectionName parameters with
        | Some "lcc" ->
            let parametersWithoutProj = removeProjParameter parameters
            
            match LambertConformalConic.extractParameters
                      parametersWithoutProj with
            | Result.Ok (lccParameters, ignoredParameters) ->
                { Projection = LambertConformalConic lccParameters;
                     IgnoredParameters = ignoredParameters }
                |> Result.Ok
            | Result.Error message ->
                InvalidProjectionParameters message |> Result.Error
        | Some "merc" ->
            { Projection = Mercator;
                 IgnoredParameters = removeProjParameter parameters }
            |> Result.Ok
        | Some unsupportedProjection ->
            unsupportedProjection |> UnsupportedProjection |> Result.Error
        | None -> Result.Error ProjectionNotSpecified
    | Result.Error parsingError ->
        parsingError |> SpecParsingError |> Result.Error
