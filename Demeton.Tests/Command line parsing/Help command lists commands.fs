module Tests.``Command line parsing``.``Help command lists commands``

open CommandLine
open CommandLine.Common

open Xunit
open Swensen.Unquote

let supportedCommands: Command[] = [|
    { Name = "potato"; ShortDescription = "boils a potato"; 
        Parameters = [| |];
        Runner = fun _ -> CommandExecuted }
    { Name = "tomato"; ShortDescription = "peels a tomato";
        Parameters = [|  |];
        Runner = fun _ -> CommandExecuted }
|]

let mutable helpOutput: string option = None

let writeHelpOutputToString output = helpOutput <- Some output

[<Fact>]
let ``Without arguments it returns normally``() =
    let result = 
        HelpCommand.runCommand 
            "someapp" supportedCommands writeHelpOutputToString []
    test <@ result = CommandExecuted @>

[<Fact>]
let ``Without arguments it lists available commands and their descriptions``() =
    let _ = 
        HelpCommand.runCommand 
            "someapp" supportedCommands writeHelpOutputToString []
    test <@ helpOutput = Some @"USAGE: someapp <command> {<command parameters>}

AVAILABLE COMMANDS:
potato: boils a potato
tomato: peels a tomato
help: displays help information (this command)

HINT: for a detailed help about a certain command, specify the command name, like:
someapp help potato" @>
