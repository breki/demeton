module Tests.``Command line parsing``.``Rendering general help``

open CommandLine
open CommandLine.Common
open Text
open System.Text

open Xunit
open Swensen.Unquote

let supportedCommands: Command[] = [|
    { Name = "potato"; ShortDescription = "boils a potato"; 
        Description = "";
        Parameters = [| |];
        Runner = fun _ -> CommandExecuted }
    { Name = "tomato"; ShortDescription = "peels a tomato";
        Description = "";
        Parameters = [|  |];
        Runner = fun _ -> CommandExecuted }
|]

let mutable helpOutputBuilder = StringBuilder()
let mutable errorOutputBuilder = StringBuilder()

let initOutput() = 
    helpOutputBuilder <- StringBuilder()
    errorOutputBuilder <- StringBuilder()

let writeHelpOutput text =
    helpOutputBuilder |> append text |> ignore

let writeErrorOutput text =
    errorOutputBuilder |> append text |> ignore

let helpOutput() = helpOutputBuilder.ToString()
let errorOutput() = errorOutputBuilder.ToString()

[<Fact>]
let ``Without arguments it returns normally``() =
    initOutput()

    let result = 
        HelpCommand.run "someapp" supportedCommands 
            writeHelpOutput writeErrorOutput []
    test <@ result = CommandExecuted @>

[<Fact>]
let ``Without arguments it lists available commands and their descriptions``() =
    initOutput()

    let _ = 
        HelpCommand.run "someapp" supportedCommands 
            writeHelpOutput writeErrorOutput []
    test <@ helpOutput() = @"USAGE: someapp <command> {<command parameters>}

AVAILABLE COMMANDS:
potato: boils a potato
tomato: peels a tomato
help: displays help information (this command)

HINT: for a detailed help about a certain command, specify the command name, like:
someapp help potato

" @>

[<Fact>]
let ``Reports an error on unrecognized command``() =
    initOutput()

    let pars = [
        ParsedArg { Name = "command"; Value = "unknowncmd" }
    ]

    let result = 
        HelpCommand.run 
            "someapp" supportedCommands writeHelpOutput 
            writeErrorOutput pars
    test <@ errorOutput() = 
                "Unrecognized command 'unknowncmd'. Please use 'help' "+
                "command without arguments to list all available commands." @>
    test <@ result = UnregnizedCommand @>
