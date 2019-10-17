[<RequireQualifiedAccess>]
module CommandLine.HelpCommand

open Common

open System.Text

let appendLine text (sb: StringBuilder) =
    sb.AppendLine(text)

let appendLines lines (sb: StringBuilder) =
    lines |> Seq.fold (fun builder line -> builder |> appendLine line) sb

let appendFormat 
    format 
    ([<System.ParamArray>] args: obj []) 
    (sb: StringBuilder) 
    = 
    sb.AppendFormat (format, args)

let newLine (sb: StringBuilder) = sb.AppendLine()

let toString (sb: StringBuilder) = sb.ToString()

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

    StringBuilder()
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
