module Tests.``Command line parsing``.``Parsing commands``

open CommandLine.Common

open Xunit
open Swensen.Unquote

type CommandRunner = ParsedParameters -> ParsingResult

type CommandLineCommand = {
    Name: string
    Runner: CommandRunner
    }

let parseAndExecuteCommandLine args supportedCommands = 
    //let commandName = args.[0]

    //supportedCommands |> Array.find (fun cmd -> cmd)
    0

type TestOptions1 = { Value: int }
type TestOptions2 = { Value: string }

let par1Parser name: OptionValueParsingResult = 
    OkValue 1
    //let (_, oldOptions) = context

    //Ok (context 
    //    |> withOptions { oldOptions with Value = oldOptions.Value + 1 })

let par2Parser name: OptionValueParsingResult = 
    OkValue 2
    //let (_, oldOptions) = context

    //Ok (context 
    //    |> withOptions { oldOptions with Value = oldOptions.Value + "x" })

let mutable command1Executed = false

let command1Runner args =
    invalidOp "todo"

let command2Runner args =
    invalidOp "todo"

let supportedCommands: CommandLineCommand[] = [|
    { Name = "cmd1"; Runner = command1Runner }
    { Name = "cmd2"; Runner = command2Runner }
|]

[<Fact(Skip="todo")>]
let ``Parses a whole command from the command line``() =
    let args = [ "cmd1"; "--par1" ]

    let result = parseAndExecuteCommandLine args supportedCommands 
    test <@ result = 0 @>
    test <@ command1Executed @>

