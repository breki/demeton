/// <summary>
/// Contains types and functions for parsing command line parameters.
/// </summary>
module Demeton.CommandLineParsing

open System
open System.Globalization

/// <summary>
/// A tuple holding the current context of the parser. The first item is a list
/// of command line arguments that have not been consumed yet by the parser.
/// The second item is the generic options type into which the parser stores 
/// parsed information.
/// </summary>
type ParsingContext<'TOptions> = string list * 'TOptions

/// <summary>
/// The result of parsing. In case the parsing was successful, it contains
/// parsing context. In case of errors, the object contains an error message.
/// </summary>
type ParsingResult<'TOptions> = Result<ParsingContext<'TOptions>, string>

type CommandLineParsingFunction<'TOptions> = 
    string -> ParsingContext<'TOptions> -> ParsingResult<'TOptions>

type CommandLineParameter<'TOptions> = {
    Name: string
    Parser: CommandLineParsingFunction<'TOptions>
    }

/// <summary>
/// Fetches the next argument from the arguments list stored in the parsing 
/// context. Returns <c>None</c> if there are no arguments left.
/// </summary>
let nextArg (context: ParsingContext<'TOptions>) =
    let (args, _) = context 
    match args.Length with
    | 0 -> None
    | _ -> Some (args |> List.head)

/// <summary>
/// Fetches the next argument from the arguments list stored in the parsing 
/// context, which is itself inside the parsing result. Returns <c>None</c> if 
/// there are no arguments left. In case the parsing result is an error, throws
/// an exception.
/// </summary>
let nextArgResult (result: ParsingResult<'TOptions>)
    : (string option * ParsingContext<'TOptions>) =
    match result with
    | Ok (args, options) -> 
        match args.Length with
        | 0 -> (None, ([], options))
        | _ -> (Some (args |> List.head), (args |> List.tail, options))
    | _ -> invalidOp "Parsing is already in a failed state."

/// <summary>
/// Consumes the first argument in the parsing context's arguments list
/// and returns a new parsing context with the remaining arguments.
/// </summary>
let consumeArg (context: ParsingContext<'TOptions>) =
    let (args, result) = context 
    (args |> List.tail, result)

/// <summary>
/// Indicates whether there are remaining arguments to parse from the parsing
/// context.
/// </summary>
let hasMoreArgs (context: ParsingResult<'TOptions>) =
    match context with
    | Ok (args, _) -> args.Length > 0
    | Error _ -> false

/// <summary>
/// Constructs a parsing result indicating a parsing error with the specified
/// error message.
/// </summary>
let withError errorMessage (_: ParsingContext<'TOptions>) =
    Error errorMessage


/// <summary>
/// Creates a new parsing context with the updated options.
/// </summary>
let withOptions 
    (updatedOptions: 'TOptions) 
    (context: ParsingContext<'TOptions>)
    : ParsingContext<'TOptions> =
    let (args, _) = context
    (args, updatedOptions)

/// <summary>
/// Constructs a parsing result indicating a missing parameter value error.
/// </summary>
let parameterValueIsMissing parameter context =
    let message = (sprintf "'%s' parameter's value is missing." parameter)
    context |> withError message


/// <summary>
/// Constructs a parsing result indicating an invalid parameter value error.
/// </summary>
let invalidParameter parameter reason context =
    let message = 
        sprintf "'%s' parameter's value is invalid, %s." parameter reason
    context |> withError message

/// <summary>
/// Parses a parameter value using the provided <see cref="parseValue" />
/// function. If there is no parameter value, the function indicates that
/// with an error parsing result.
/// </summary>
let parseParameterValue 
    parseValue parameterName (context: ParsingContext<'TOptions>) =
    match nextArg context with
    | None -> context |> parameterValueIsMissing parameterName
    | (Some x) when x.StartsWith("--") ->
        context |> parameterValueIsMissing parameterName
    | Some parameterValue -> parseValue parameterValue context

/// <summary>
/// Tries to parse a float value from the string. Returns <c>None</c> if 
/// parsing was not successful.
/// </summary>
let tryParseFloat (value: string) =
    match Double.TryParse
        (
        value,
        NumberStyles.Float,
        CultureInfo.InvariantCulture) with
    | (true, parsed) -> Some parsed
    | _ -> None
