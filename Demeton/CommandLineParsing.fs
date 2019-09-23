/// <summary>
/// Contains types and functions for parsing command line parameters.
/// </summary>
module Demeton.CommandLineParsing

/// <summary>
/// A tuple holding the current context of the parser. The first item is a list
/// of command line arguments that have not been consumed yet by the parser.
/// The second item is the generic options type into which the parser stores 
/// parsed information.
/// </summary>
type ParsingContext<'TOptions> = string list * 'TOptions
type ParsingResult<'TOptions> = Result<ParsingContext<'TOptions>, string>


let nextArg (context: ParsingContext<'TOptions>) =
    let (args, _) = context 
    match args.Length with
    | 0 -> None
    | _ -> Some (args |> List.head)


let nextArgResult (result: ParsingResult<'TOptions>)
    : (string option * ParsingContext<'TOptions>) =
    match result with
    | Ok (args, options) -> 
        match args.Length with
        | 0 -> (None, ([], options))
        | _ -> (Some (args |> List.head), (args |> List.tail, options))
    | _ -> invalidOp "Parsing is already in a failed state."


let consumeArg (context: ParsingContext<'TOptions>) =
    let (args, result) = context 
    (args |> List.tail, result)


let hasMoreArgs (context: ParsingContext<'TOptions>) =
    let (args, _) = context
    args.Length > 0


let hasMoreArgsResult (context: ParsingResult<'TOptions>) =
    match context with
    | Ok (args, _) -> args.Length > 0
    | Error _ -> false


let withError errorMessage (_: ParsingContext<'TOptions>) =
    Error errorMessage


let withOptions 
    (updatedOptions: 'TOptions) 
    (context: ParsingContext<'TOptions>)
    : ParsingContext<'TOptions> =
    let (args, _) = context
    (args, updatedOptions)


let parameterValueIsMissing parameter context =
    let message = (sprintf "'%s' parameter's value is missing." parameter)
    context |> withError message


let invalidParameter parameter reason context =
    let message = 
        sprintf "'%s' parameter's value is invalid, %s." parameter reason
    context |> withError message


let parseParameter 
    parameterName parseValue (context: ParsingContext<'TOptions>) =
    match nextArg context with
    | None -> context |> parameterValueIsMissing parameterName
    | (Some x) when x.StartsWith("--") ->
        context |> parameterValueIsMissing parameterName
    | Some parameterValue -> parseValue parameterValue context
