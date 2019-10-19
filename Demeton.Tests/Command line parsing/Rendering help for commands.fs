module Tests.``Command line parsing``.``Rendering help for commands``

open Text
open CommandLine
open CommandLine.Common

open System

open Xunit
open Swensen.Unquote

let toArgMaybe parameter =
    match parameter with
    | Arg arg -> Some arg
    | _ -> None

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

let commandArgs parameters = 
    parameters |> Array.choose toArgMaybe

let commandUsage (command: Command) =
    let argsList = commandArgs command.Parameters
    let argsMentions = 
        argsList 
        |> Array.map (fun p -> "<" + p.Name + ">") 
        |> String.concat " "

    buildString()
    |> appendFormat "USAGE: {0}" [| command.Name |]
    |> ifDo (argsList.Length > 0) 
        (fun sb -> sb |> append " " |> append argsMentions)
    |> ifDo (hasOptionsAndSwitches command.Parameters)
        (fun sb -> sb |> append " [<options>]")
    |> toString

let parametersDescriptions (parameters: CommandParameter[]) =
    let argsSection =
        parameters
        |> Array.filter isArg
        |> Array.map HelpCommand.parameterDescription 
        |> String.concat (Environment.NewLine + Environment.NewLine)

    let optionsSection =
        parameters
        |> Array.filter isOptionOrSwitch
        |> Array.sortBy parameterName
        |> Array.map HelpCommand.parameterDescription
        |> String.concat (Environment.NewLine + Environment.NewLine)

    buildString()
    |> ifDo (argsSection.Length > 0)
        (fun sb -> sb 
                    |> appendLine "ARGUMENTS:" 
                    |> append argsSection)
    |> ifDo (optionsSection.Length > 0)
        (fun sb -> sb 
                    |> appendLine "OPTIONS:" 
                    |> append optionsSection)
    |> toString

let commandDescription (command: Command) = invalidOp "todo"

let supportedParameters = [|
    Arg { 
        Name = "arg1"
        Description = "some description 1"
        Format = "format1"
        Example = None
        Parser = fun _ -> OkValue 1 }
    Arg { 
        Name = "arg2"
        Description = "some description 2"
        Format = "format2"
        Example = Some ("x1", "xx1")
        Parser = fun _ -> OkValue 1 }
    Switch {
        Name = "switch1"
        Description = "some description 3" }
    Option {
        Name = "option1" 
        Description = "some description 4"
        ValuePlaceholder = "value"
        Format = "format3"
        Default = 1
        Example = None
        Parser = fun _ -> OkValue 1 }
    |]

let noParameters = [| |]

let argumentOnly = [|
    Arg { 
        Name = "arg1"
        Description = "some description 1"
        Format = "format1"
        Example = None
        Parser = fun _ -> OkValue 1 } 
    |]

let argumentsOnly = [|
    Arg { 
        Name = "arg1"
        Description = "some description 1"
        Format = "format1"
        Example = None
        Parser = fun _ -> OkValue 1 } 
    Arg { 
        Name = "arg2"
        Description = "some description 2"
        Format = "format2"
        Example = None
        Parser = fun _ -> OkValue 1 } 
    |]

let argAndSwitch = [|
    Arg { 
        Name = "arg1"
        Description = "some description 1"
        Format = "format1"
        Example = None
        Parser = fun _ -> OkValue 1 }
    Switch {
        Name = "switch1"
        Description = "some description 3" }
    |]

let cmdTemplate = { 
    Name = "somecmd"
    ShortDescription = ""
    Parameters = [| |]
    Runner = fun _ -> CommandExecuted }

let cmdWith parameters =
    { cmdTemplate with Parameters = parameters }

[<Fact>]
let ``Supports rendering command usage without args, options and switches``() =
    test <@ commandUsage (cmdWith noParameters) =
        "USAGE: somecmd" @>

[<Fact>]
let ``Supports rendering command usage without options and switches``() =
    test <@ commandUsage (cmdWith argumentOnly) =
        "USAGE: somecmd <arg1>" @>

[<Fact>]
let ``Adds space as a separator between args``() =
    test <@ commandUsage (cmdWith argumentsOnly) =
        "USAGE: somecmd <arg1> <arg2>" @>

[<Fact>]
let ``Supports rendering command usage without args``() =    
    let optionsOnly = [|
        Switch {
            Name = "switch1"; Description = "" }
        Option {
            Name = "option1"; Description = ""
            ValuePlaceholder = ""
            Format = ""
            Default = 1
            Example = None
            Parser = fun _ -> OkValue 1 }
        |]

    test <@ commandUsage (cmdWith optionsOnly) =
        "USAGE: somecmd [<options>]" @>

[<Fact>]
let ``Supports rendering command usage with args and options``() =
    test <@ commandUsage (cmdWith argAndSwitch) =
        "USAGE: somecmd <arg1> [<options>]" @>

[<Fact>]
let ``Supports rendering parameter details when there are no parameters``() =
    test <@ parametersDescriptions noParameters = "" @>

[<Fact>]
let ``Supports rendering parameter details when there are only arguments``() =
    test <@ parametersDescriptions argumentsOnly = @"ARGUMENTS:
<arg1>: some description 1
   FORMAT: format1

<arg2>: some description 2
   FORMAT: format2" @>

[<Fact>]
let ``Supports rendering parameter details when there are only options and switches``() =
    let optionsOnly = [|
        Option {
            Name = "option1" 
            Description = "some description 4"
            ValuePlaceholder = "value"
            Format = "format3"
            Default = 1
            Example = None
            Parser = fun _ -> OkValue 1 }
        Switch {
            Name = "switch1"
            Description = "some description 3" }
        |]

    test <@ parametersDescriptions optionsOnly = @"OPTIONS:
--option1 <value>: some description 4
   FORMAT: format3
   DEFAULT VALUE: 1

--switch1: some description 3" @>

[<Fact>]
let ``Orders options and switches details alphabetically``() =
    let optionsOnlyUnsorted = [|
        Switch {
            Name = "switch1"
            Description = "some description 3" }
        Option {
            Name = "option1" 
            Description = "some description 4"
            ValuePlaceholder = "value"
            Format = "format3"
            Default = 1
            Example = None
            Parser = fun _ -> OkValue 1 }
        |]

    test <@ parametersDescriptions optionsOnlyUnsorted = @"OPTIONS:
--option1 <value>: some description 4
   FORMAT: format3
   DEFAULT VALUE: 1

--switch1: some description 3" @>