[<RequireQualifiedAccess>]
module CommandLine.HelpCommand

open Common
open Text

open System

let isArg parameter =
    match parameter with
    | Arg _ -> true
    | _ -> false

let isOptionOrSwitch parameter =
    match parameter with
    | Option _ -> true
    | Switch _ -> true
    | Arg _ -> false

let hasOptionsAndSwitches (parameters: CommandParameter[]) =
    parameters |> Array.exists isOptionOrSwitch

let parameterName (parameter: CommandParameter) =
    match parameter with
    | Arg { Name = name } -> name
    | Option { Name = name } -> name
    | Switch { Name = name } -> name

let argMention arg =
    match arg.IsMandatory with
    | true -> "<" + arg.Name + ">"
    | false -> "[<" + arg.Name + ">]"

let parameterDescription parameter: string =
    match parameter with
    | Arg arg -> 
        buildString()
        |> appendFormat "{0}: {1}" [| argMention arg ; arg.Description |]
        |> newLine
        |> appendFormat "   FORMAT: {0}" [| arg.Format |]
        |> ifDo (arg.Example <> None) (fun x -> 
            x 
            |> newLine 
            |> appendFormat "   EXAMPLE: {0} - {1}" [| 
                fst (Option.get arg.Example); snd (Option.get arg.Example) |])
        |> toString

    | Option option ->
        buildString()
        |> appendFormat 
            "{0}{1} <{2}>: {3}" 
            [| ParameterPrefix; option.Name; 
                option.ValuePlaceholder; option.Description  |]
        |> newLine
        |> appendFormat "   FORMAT: {0}" [| option.Format |]
        |> newLine
        |> appendFormat "   DEFAULT VALUE: {0}" [| option.Default |]
        |> ifDo (option.Example <> None) (fun x -> 
            x 
            |> newLine 
            |> appendFormat "   EXAMPLE: {0}{1} {2} - {3}" [| 
                ParameterPrefix; option.Name; 
                fst (Option.get option.Example); snd (Option.get option.Example) |])
        |> toString
    
    | Switch switch ->
        buildString()
        |> appendFormat "{0}{1}: {2}" [| 
            ParameterPrefix; switch.Name; switch.Description |]
        |> toString


let parametersDescriptions (parameters: CommandParameter[]) =
    let argsSection =
        parameters
        |> Array.filter isArg
        |> Array.map parameterDescription 
        |> String.concat (Environment.NewLine + Environment.NewLine)

    let optionsSection =
        parameters
        |> Array.filter isOptionOrSwitch
        |> Array.sortBy parameterName
        |> Array.map parameterDescription
        |> String.concat (Environment.NewLine + Environment.NewLine)

    let hasArgs = argsSection.Length > 0
    let hasOptions = optionsSection.Length > 0

    buildString()
    |> ifDo hasArgs
        (fun sb -> sb 
                    |> appendLine "ARGUMENTS:" 
                    |> append argsSection)
    |> ifDo hasOptions
        (fun sb -> sb 
                    |> ifDo hasArgs (newLine >> newLine)
                    |> appendLine "OPTIONS:" 
                    |> append optionsSection)
    |> toString

let commandShortDescription (command: Command) =
    sprintf "%s: %s" command.Name command.ShortDescription

let commandUsage (command: Command) =
    let toArgMaybe parameter =
        match parameter with
        | Arg arg -> Some arg
        | _ -> None

    let commandArgs parameters = 
        parameters |> Array.choose toArgMaybe

    let argsList = commandArgs command.Parameters

    let argsMentions = 
        argsList |> Array.map argMention |> String.concat " "

    buildString()
    |> appendFormat "USAGE: {0}" [| command.Name |]
    |> ifDo (argsList.Length > 0) 
        (fun sb -> sb |> append " " |> append argsMentions)
    |> ifDo (hasOptionsAndSwitches command.Parameters)
        (fun sb -> sb |> append " [<options>]")
    |> toString

let commandDescription (command: Command) = 
    buildString()
    |> appendFormat "{0}: {1}" [| command.Name; command.ShortDescription |]
    |> newLine
    |> newLine
    |> appendLine command.Description
    |> newLine
    |> appendLine (commandUsage command)
    |> newLine
    |> append (parametersDescriptions command.Parameters)
    |> toString

let helpMainHelp executableName supportedCommands =
    buildString()
    |> appendFormat
        "USAGE: {0} <command> {{<command parameters>}}" [| executableName |]
    |> newLine
    |> newLine
    |> appendLine "AVAILABLE COMMANDS:"
    |> appendLines 
        (supportedCommands |> Array.map commandShortDescription)
    |> newLine
    |> appendLine 
        "HINT: for a detailed help about a certain command, specify the command name, like:"
    |> appendFormat
        "{0} help {1}" 
            [| executableName; supportedCommands.[0].Name |]
    |> newLine
    |> newLine
    |> toString

let helpCommandTemplateDef = {
    Name = "help";
    ShortDescription = "displays help information (this command)"
    Description = 
        "Displays the help information about the command line interface."
    Parameters = [|
        Arg.build "command" |> Arg.optional
        |> Arg.desc 
            ("The name of the command whose detailed help is to be shown. "
            + "If not specified, help command will display the list of all "
            + "available commands with their short descriptions.")
        |> Arg.toPar
    |]
    Runner = fun _ -> CommandExecuted };

type Options = {
    Command: string option
}

let fillOptions parsedParameters =
    let defaultOptions = { Command = None }

    let processParameter options parameter =
        match parameter with
        | ParsedArg { Name = "command"; Value = value } -> 
            { options with Command = Some (value :?> string) }
        | _ -> invalidOp "Unrecognized parameter."

    parsedParameters 
    |> List.fold processParameter defaultOptions

let runCommandFromOptions
    (options: Options)
    (executableName: string) 
    supportedCommands 
    writeHelpOutput 
    (writeErrorOutput: string -> unit)
    =
    let supportedCommandsIncludingHelp = 
        Array.append supportedCommands [| helpCommandTemplateDef |]

    let (helpOutput, errorOutput, commandResult) = 
        match options.Command with
        | None -> 
            (helpMainHelp executableName supportedCommandsIncludingHelp, 
                "", CommandExecuted)
        | Some commandName -> 
            let commandMaybe = 
                supportedCommandsIncludingHelp 
                |> tryFindCommand commandName
            match commandMaybe with
            | Some command -> 
                (commandDescription command, "", CommandExecuted)
            | None -> 
                ("",
                    sprintf 
                        "Unrecognized command '%s'. Please use 'help' command without arguments to list all available commands."
                        commandName,
                    UnregnizedCommand)

    match helpOutput, errorOutput, commandResult with
    | _, "", _ -> 
        writeHelpOutput helpOutput
        commandResult
    | "", _, _ ->
        writeErrorOutput errorOutput
        commandResult
    | _ -> commandResult

let run 
    executableName 
    supportedCommands 
    writeHelpOutput
    (writeErrorOutput: string -> unit)
    parsedParameters =
    let options = fillOptions parsedParameters
    runCommandFromOptions
        options executableName supportedCommands writeHelpOutput writeErrorOutput
