/// <summary>
/// Contains types and functions for parsing command line parameters.
/// </summary>
module CommandLine.Common

open CommandLine

open System
open System.Globalization
open System.IO

[<Literal>]
let ParameterPrefix = "--"

type OptionValueParsingResult = 
    | OkValue of Object
    | InvalidValue of string

type OptionValueParser = string -> OptionValueParsingResult


type CommandSwitch = { Name: string }
type CommandOption = { Name: string; Parser: OptionValueParser }

type CommandParameter = 
    | Switch of CommandSwitch
    | Option of CommandOption

type ParsedSwitch = { Name: string }
type ParsedOption = { Name: string; Value: Object }

type ParsedParameter =
    | ParsedSwitch of ParsedSwitch
    | ParsedOption of ParsedOption

type ParsedParameters = ParsedParameter list

type ParsingState = 
    | ParsingSuccess of ParsedParameter list
    | ParsingInProgress of (string list * ParsedParameters)
    | ParsingFail of string

type ParsingResult = Result<ParsedParameters, string>
      

/// <summary>
/// Fetches the next argument from the arguments list stored in the parsing 
/// context. Returns <c>None</c> if there are no arguments left.
/// </summary>
let nextArg (state: ParsingState): (string option * ParsingState) =

    let consumeArg state = 
        match state with    
        | ParsingInProgress (args, parsedParameters) ->
            match args.Length with
            | 0 -> ParsingSuccess parsedParameters
            | _ -> 
                ParsingInProgress (args |> List.tail, parsedParameters)
        | ParsingSuccess _ -> invalidOp "should not be called in this state"
        | ParsingFail _ -> invalidOp "should not be called in this state"

    match state with
    | ParsingInProgress (args, _) ->
        match args.Length with
        | 0 -> (None, consumeArg state)
        | _ -> 
            (Some (args |> List.head), consumeArg state)
    | _ -> invalidOp "should not be called in this state"


/// <summary>
/// Constructs a parsing result indicating a missing parameter value error.
/// </summary>
let parameterValueIsMissing parameter =
    let message = (sprintf "'%s' parameter's value is missing." parameter)
    ParsingFail message


/// <summary>
/// Constructs a parsing result indicating an invalid parameter value error.
/// </summary>
let invalidParameter parameter reason =
    let message = 
        sprintf "'%s' parameter's value is invalid, %s." parameter reason
    ParsingFail message


let appendParameter parsedParameter state =
    match state with
    | ParsingInProgress (args, parsedParameters) ->
        ParsingInProgress (args, parsedParameter :: parsedParameters)
    | _ -> invalidOp "should not be called in this state"


/// <summary>
/// Parses a parameter value using the provided <see cref="parseValue" />
/// function. If there is no parameter value, the function indicates that
/// with an error parsing result.
/// </summary>
let parseOptionValue 
    (parseValue: OptionValueParser)
    parameterName 
    (state: ParsingState)
    : ParsingState =
    match nextArg state with
    | (None, _) -> parameterValueIsMissing parameterName
    | (Some value, _) when value.StartsWith(ParameterPrefix) ->
        parameterValueIsMissing parameterName
    | (Some value, stateConsumed) -> 
        let result = parseValue value
        match result with
        | InvalidValue reason -> invalidParameter parameterName reason
        | OkValue value -> 
            stateConsumed 
            |> appendParameter 
                (ParsedOption { Name = parameterName; Value = value })

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


let findParameterByName 
    parameterName (supportedParameters: CommandParameter[]) =

    let hasName (parameter: CommandParameter) =
        match parameter with
        | CommandParameter.Switch switch -> 
            String.Equals(
                parameterName, 
                switch.Name, 
                StringComparison.OrdinalIgnoreCase)
        | CommandParameter.Option option ->
            String.Equals(
                parameterName, 
                option.Name, 
                StringComparison.OrdinalIgnoreCase)

    supportedParameters |> Array.tryFind hasName

let parseParameters 
    (args: string list) 
    (supportedParameters: CommandParameter[])
    : ParsingResult =

    let parsingInProgress state =
        match state with
        | ParsingInProgress _ -> true
        | _ -> false

    let mutable state: ParsingState = ParsingInProgress (args, [])

    while parsingInProgress state do
        let (arg, consumedState) = nextArg state

        state <-
            match (arg, consumedState) with
            | (Some argParameter, _) 
                when argParameter.StartsWith(ParameterPrefix) ->

                let parameterName = argParameter.Substring 2
                let parameterMaybe = 
                    supportedParameters |> findParameterByName parameterName
                match parameterMaybe with
                | Some (CommandParameter.Option option) -> 
                    parseOptionValue option.Parser parameterName consumedState
                | Some (CommandParameter.Switch switch) -> 
                    consumedState 
                    |> appendParameter (ParsedSwitch { Name = parameterName })
                | None -> 
                    ParsingFail 
                        (sprintf "Unrecognized parameter '%s'." parameterName)
            | (Some argParameter, _) -> 
                ParsingFail 
                    (sprintf "Unrecognized parameter '%s'." argParameter)
            | (None, ParsingSuccess _) -> consumedState
            | _ -> invalidOp "BUG: this should never happen"

    match state with
    | ParsingSuccess parsedParameters -> 
        Ok (parsedParameters |> List.rev)
    | ParsingFail message -> Error message
    | _ -> invalidOp "BUG: this should never happen"
