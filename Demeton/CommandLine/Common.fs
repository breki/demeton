/// <summary>
/// Contains types and functions for parsing command line parameters.
/// </summary>
module CommandLine.Common

open System
open System.Globalization

[<Literal>]
let ParameterPrefix = "--"

type OptionValueParsingResult = 
    | OkValue of Object
    | InvalidValue of string

type OptionValueParser = string -> OptionValueParsingResult

type CommandArg = { Name: string; Parser: OptionValueParser }
type CommandSwitch = { Name: string }
type CommandOption = { Name: string; Parser: OptionValueParser }

type CommandParameter = 
    | Arg of CommandArg
    | Switch of CommandSwitch
    | Option of CommandOption

type ParsedArg = { Name: string; Value: Object }
type ParsedSwitch = { Name: string }
type ParsedOption = { Name: string; Value: Object }

type ParsedParameter =
    | ParsedArg of ParsedArg
    | ParsedSwitch of ParsedSwitch
    | ParsedOption of ParsedOption

type ParsedParameters = ParsedParameter list

type ParsingState = 
    | NoMoreArgs of ParsedParameter list
    | ParsingInProgress of (string list * ParsedParameters)
    | ParsingFail of string

type ParsingResult = Result<ParsedParameters, string>

type CommandResult = 
    | CommandExecuted
    | ParsingFailed
    | CommandNotFound

type CommandRunner = ParsedParameters -> CommandResult

type Command = {
    Name: string
    ShortDescription: string
    Parameters: CommandParameter[]
    Runner: CommandRunner
    }
      

/// <summary>
/// Fetches the next argument from the arguments list stored in the parsing 
/// context. Returns <c>None</c> if there are no arguments left.
/// </summary>
let nextArg (state: ParsingState): (string option * ParsingState) =

    let consumeArg state = 
        match state with    
        | ParsingInProgress ([], parsedParameters) ->
            NoMoreArgs parsedParameters
        | ParsingInProgress (args, parsedParameters) ->
            ParsingInProgress (args |> List.tail, parsedParameters)
        | NoMoreArgs _ -> invalidOp "should not be called in this state"
        | ParsingFail _ -> invalidOp "should not be called in this state"

    match state with
    | ParsingInProgress ([], _) -> (None, consumeArg state)
    | ParsingInProgress (args, _) -> 
        (Some (args |> List.head), consumeArg state)
    | _ -> invalidOp "should not be called in this state"


/// <summary>
/// Constructs a parsing result indicating a missing option's value error.
/// </summary>
let optionValueIsMissing parameter =
    let message = (sprintf "'%s' option's value is missing." parameter)
    ParsingFail message

let argumentValueIsMissing name =
    let message = (sprintf "<%s> argument's value is missing." name)
    ParsingFail message

/// <summary>
/// Constructs a parsing result indicating an invalid option's value error.
/// </summary>
let invalidOptionValue name reason =
    let message = 
        sprintf "'%s' option's value is invalid, %s." name reason
    ParsingFail message

let invalidArgumentValue name reason =
    let message = 
        sprintf "<%s> argument's value is invalid, %s." name reason
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
    | (None, _) -> optionValueIsMissing parameterName
    | (Some value, _) when value.StartsWith(ParameterPrefix) ->
        optionValueIsMissing parameterName
    | (Some value, stateConsumed) -> 
        let result = parseValue value
        match result with
        | InvalidValue reason -> invalidOptionValue parameterName reason
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
        | CommandParameter.Arg _ -> false

    supportedParameters |> Array.tryFind hasName

let validateSupportedParameters 
    args
    (supportedParameters: CommandParameter[]) =
    let anyWronglyOrderedParameters = 
        supportedParameters 
        |> Array.pairwise
        |> Array.tryFind (fun x -> 
            match x with 
            | (CommandParameter.Switch _, CommandParameter.Arg _) -> true
            | (CommandParameter.Option _, CommandParameter.Arg _) -> true
            | _ -> false
            )

    match anyWronglyOrderedParameters with
    | None -> ParsingInProgress (args, [])
    | _ -> ParsingFail "All command arguments need to be specified before any options and switches."

let parseParameters 
    (args: string list) 
    (supportedParameters: CommandParameter[])
    : ParsingResult =

    let parsingInProgress state =
        match state with
        | ParsingInProgress _ -> true
        | _ -> false

    let isCommandArg = 
        function
        | CommandParameter.Arg _ -> true 
        | _ -> false

    let commandArgsCount() =
        supportedParameters |> Array.filter isCommandArg |> Array.length

    let nextCommandArgumentToFindBasedOnParsed 
        (parsedParameters: ParsedParameters) = 
        let cmdArgsCount = commandArgsCount()
        match parsedParameters.Length < cmdArgsCount with
        | true -> 
            let commandArgIndex = parsedParameters.Length
            Some supportedParameters.[commandArgIndex]
        | false -> None
        
    let nextCommandArgumentToFind =
        function
        | ParsingInProgress (_, parsedParameters) ->
            nextCommandArgumentToFindBasedOnParsed parsedParameters
        | NoMoreArgs parsedParameters -> 
            nextCommandArgumentToFindBasedOnParsed parsedParameters
        | ParsingFail _ -> invalidOp "should not be called in this state"

    let consumeNextCommandArg 
        (argMaybe: string option) 
        (cmdArg: CommandParameter)
        state =
        match (argMaybe, cmdArg, state) with
        | (None, CommandParameter.Arg { Name = argName }, _) -> 
            argumentValueIsMissing argName
        | (Some arg, CommandParameter.Arg { Name = argName }, _) 
            when arg.StartsWith ParameterPrefix ->
            argumentValueIsMissing argName
        | (Some arg, CommandParameter.Arg { Name = argName; Parser = parser }, _) -> 
            match parser arg with
            | OkValue value -> 
                state 
                |> appendParameter (ParsedArg { Name = argName; Value = value })
            | InvalidValue reason -> invalidArgumentValue argName reason
        | _ -> invalidOp "todo"

    let handleNextArg (arg: string option) consumedState =
        match nextCommandArgumentToFind consumedState with
        | Some cmdArgument -> 
            consumeNextCommandArg arg cmdArgument consumedState
        | None ->
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
                    |> appendParameter (ParsedSwitch { Name = switch.Name })
                | Some (CommandParameter.Arg _) ->
                    invalidOp "should this ever happen?"
                | None -> 
                    ParsingFail 
                        (sprintf "Unrecognized parameter '%s'." parameterName)
            | (Some argParameter, _) -> 
                ParsingFail 
                    (sprintf "Unrecognized parameter '%s'." argParameter)
            | (None, NoMoreArgs _) -> consumedState
            | _ -> invalidOp "BUG: this should never happen"

    let mutable state: ParsingState = 
        validateSupportedParameters args supportedParameters

    while parsingInProgress state do
        let (arg, consumedState) = nextArg state
        state <- handleNextArg arg consumedState

    match state with
    | NoMoreArgs parsedParameters -> 
        Ok (parsedParameters |> List.rev)
    | ParsingFail message -> Error message
    | _ -> invalidOp "BUG: this should never happen"



let parseAndExecuteCommandLine (args: string[]) supportedCommands = 
    let tryFindCommand commandName =
        supportedCommands |> Array.tryFind (fun cmd -> cmd.Name = commandName)
    
    let commandName = args.[0]
    let commandMaybe = tryFindCommand commandName

    match commandMaybe with
    | Some command -> 
        let commandArgs = args |> Array.tail |> Array.toList

        let parsingResult = parseParameters commandArgs command.Parameters
        match parsingResult with
        | Ok parsedParameters -> command.Runner parsedParameters
        | Error _ -> ParsingFailed
    | None -> CommandNotFound
