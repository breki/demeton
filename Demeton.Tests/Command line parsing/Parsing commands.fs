module Tests.``Command line parsing``.``Parsing commands``

open CommandLine
open CommandLine.Common

open Xunit
open Swensen.Unquote

let parseAndExecuteCommandLine args supportedCommands = 
    Shell.parseAndExecuteCommandLine 
        (fun _ -> ())
        ""
        args 
        supportedCommands 

let mutable command1Executed = false

let command1Runner _ =
    command1Executed <- true
    CommandExecuted

let command2Runner _ = CommandExecuted

let supportedCommands: Command[] = [|
    {   Name = "cmd1" 
        ShortDescription = ""
        Description = ""
        Parameters = [| Switch.build "par1" |> Switch.toPar |]
        Runner = command1Runner }
    {   Name = "cmd2" 
        ShortDescription = ""
        Description = ""
        Parameters = [|  |]
        Runner = command2Runner }
|]

[<Fact>]
let ``If no command is specified, run help command``() =
    let args = [||]

    let result = parseAndExecuteCommandLine args supportedCommands 
    test <@ result = CommandExecuted @>
    test <@ not command1Executed @>

[<Fact>]
let ``Parses a whole command from the command line and executes is successfully``() =
    let args = [| "cmd1"; "--par1" |]

    let result = parseAndExecuteCommandLine args supportedCommands 
    test <@ result = CommandExecuted @>
    test <@ command1Executed @>

[<Fact>]
let ``Command parsing failure results in its own result code``() =
    let args = [| "cmd1"; "--par2" |]

    let result = parseAndExecuteCommandLine args supportedCommands 
    test <@ result = ParsingFailed @>
    test <@ not command1Executed @>

[<Fact>]
let ``When command was not found this results in its own result code``() =
    let args = [| "cmdX" |]

    let result = parseAndExecuteCommandLine args supportedCommands 
    test <@ result = CommandNotFound @>
    
