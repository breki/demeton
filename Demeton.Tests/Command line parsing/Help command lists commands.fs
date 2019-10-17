module Tests.``Command line parsing``.``Help command lists commands``

open CommandLine.Common

open System.Text

open Xunit
open Swensen.Unquote

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

let runHelpCommand 
    (executableName: string) supportedCommands writeHelpOutput parsedParameters =

    System.Text.StringBuilder()
    |> appendFormat
        "USAGE: {0} <command> {{<command parameters>}}" [| executableName |]
    |> newLine
    |> newLine
    |> appendLine "AVAILABLE COMMANDS:"
    |> appendLines (supportedCommands |> Array.map commandShortDescription)
    |> newLine
    |> appendLine 
        "HINT: for a detailed help about a certain command, specify the command name, like:"
    |> appendFormat
        "{0} help {1}" [| executableName; supportedCommands.[0].Name |]
    |> toString
    |> writeHelpOutput

    CommandExecuted

let supportedCommands: Command[] = [|
    { Name = "potato"; ShortDescription = "boils a potato"; 
        Parameters = [| |];
        Runner = fun _ -> CommandExecuted }
    { Name = "tomato"; ShortDescription = "peels a tomato";
        Parameters = [|  |];
        Runner = fun _ -> CommandExecuted }
    { Name = "help"; 
        ShortDescription = "displays help information (this command)";
        Parameters = [|  |];
        Runner = fun _ -> CommandExecuted }
|]

let mutable helpOutput: string option = None

let writeHelpOutputToString output = helpOutput <- Some output

[<Fact>]
let ``Without arguments it returns normally``() =
    let result = 
        runHelpCommand "someapp" supportedCommands writeHelpOutputToString []
    test <@ result = CommandExecuted @>

[<Fact>]
let ``Without arguments it lists available commands and their descriptions``() =
    let _ = 
        runHelpCommand "someapp" supportedCommands writeHelpOutputToString []
    test <@ helpOutput = Some @"USAGE: someapp <command> {<command parameters>}

AVAILABLE COMMANDS:
potato: boils a potato
tomato: peels a tomato
help: displays help information (this command)

HINT: for a detailed help about a certain command, specify the command name, like:
someapp help potato" @>
