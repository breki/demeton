module Tests.``Command line parsing``.``Parsing commands``

open Demeton.CommandLineParsing

open Xunit
open Swensen.Unquote

type CommandParserAndExecutor = string list -> int

type CommandLineCommand = {
    Name: string
    ParserAndExecutor: CommandParserAndExecutor
    }

let parseAndExecuteCommandLine args supportedCommands = 
    //let commandName = args.[0]

    //supportedCommands |> Array.find (fun cmd -> cmd)
    0

type TestOptions1 = { Value: int }
type TestOptions2 = { Value: string }

let par1Parser name context: ParsingResult<TestOptions1> = 
    let (_, oldOptions) = context

    Ok (context 
        |> withOptions { oldOptions with Value = oldOptions.Value + 1 })

let par2Parser name context: ParsingResult<TestOptions2> = 
    let (_, oldOptions) = context

    Ok (context 
        |> withOptions { oldOptions with Value = oldOptions.Value + "x" })

let mutable command1Executed = false

let command1ParserAndExecutor args =
    invalidOp "todo"

let command2ParserAndExecutor args =
    invalidOp "todo"

let supportedCommands: CommandLineCommand[] = [|
    { Name = "cmd1"; ParserAndExecutor = command1ParserAndExecutor }
    { Name = "cmd2"; ParserAndExecutor = command2ParserAndExecutor }
    //{ Name = "cmd1"; Parameters = { Name = "par1"; Parser = par1Parser }}
    //{ Name = "cmd2"; Parameters = { Name = "par2"; Parser = par2Parser }}
|]

[<Fact(Skip="todo")>]
let ``Parses a whole command from the command line``() =
    let args = [ "cmd1"; "--par1" ]

    let result = parseAndExecuteCommandLine args supportedCommands 
    test <@ result = 0 @>
    test <@ command1Executed @>

