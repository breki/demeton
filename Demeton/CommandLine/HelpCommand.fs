[<RequireQualifiedAccess>]
module CommandLine.HelpCommand

open Common
open Text

let parameterDescription parameter: string =
    match parameter with
    | Arg arg -> 
        buildString()
        |> appendFormat "<{0}>: {1}" [| arg.Name; arg.Description |]
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

let commandShortDescription (command: Command) =
    sprintf "%s: %s" command.Name command.ShortDescription

let helpCommandTemplateDef = {
    Name = "help";
    ShortDescription = "displays help information (this command)";
    Parameters = [| |]
    Runner = fun _ -> CommandExecuted };

let runCommand
    (executableName: string) supportedCommands writeHelpOutput parsedParameters 
    =
    let supportedCommandsIncludingHelp = 
        Array.append supportedCommands [| helpCommandTemplateDef |]

    buildString()
    |> appendFormat
        "USAGE: {0} <command> {{<command parameters>}}" [| executableName |]
    |> newLine
    |> newLine
    |> appendLine "AVAILABLE COMMANDS:"
    |> appendLines 
        (supportedCommandsIncludingHelp |> Array.map commandShortDescription)
    |> newLine
    |> appendLine 
        "HINT: for a detailed help about a certain command, specify the command name, like:"
    |> appendFormat
        "{0} help {1}" 
            [| executableName; supportedCommandsIncludingHelp.[0].Name |]
    |> toString
    |> writeHelpOutput

    CommandExecuted
