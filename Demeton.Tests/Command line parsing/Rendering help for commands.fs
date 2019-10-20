module Tests.``Command line parsing``.``Rendering help for commands``

open CommandLine
open CommandLine.Common
open Text

open Xunit
open Swensen.Unquote

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
    ShortDescription = "some short description"
    Description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    Parameters = [| |]
    Runner = fun _ -> CommandExecuted }

let cmdWith parameters =
    { cmdTemplate with Parameters = parameters }

[<Fact>]
let ``Supports rendering command usage without args, options and switches``() =
    test <@ HelpCommand.commandUsage (cmdWith noParameters) =
        "USAGE: somecmd" @>

[<Fact>]
let ``Supports rendering command usage without options and switches``() =
    test <@ HelpCommand.commandUsage (cmdWith argumentOnly) =
        "USAGE: somecmd <arg1>" @>

[<Fact>]
let ``Adds space as a separator between args``() =
    test <@ HelpCommand.commandUsage (cmdWith argumentsOnly) =
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

    test <@ HelpCommand.commandUsage (cmdWith optionsOnly) =
        "USAGE: somecmd [<options>]" @>

[<Fact>]
let ``Supports rendering command usage with args and options``() =
    test <@ HelpCommand.commandUsage (cmdWith argAndSwitch) =
        "USAGE: somecmd <arg1> [<options>]" @>

[<Fact>]
let ``Supports rendering parameter details when there are no parameters``() =
    test <@ HelpCommand.parametersDescriptions noParameters = "" @>

[<Fact>]
let ``Supports rendering parameter details when there are only arguments``() =
    test <@ HelpCommand.parametersDescriptions argumentsOnly = @"ARGUMENTS:
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

    test <@ HelpCommand.parametersDescriptions optionsOnly = @"OPTIONS:
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

    test <@ HelpCommand.parametersDescriptions optionsOnlyUnsorted = @"OPTIONS:
--option1 <value>: some description 4
   FORMAT: format3
   DEFAULT VALUE: 1

--switch1: some description 3" @>

[<Fact>]
let ``Can render a combination of arguments and options``() =
    let parameters = [|
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

    test <@ HelpCommand.parametersDescriptions parameters = @"ARGUMENTS:
<arg1>: some description 1
   FORMAT: format1

<arg2>: some description 2
   FORMAT: format2
   EXAMPLE: x1 - xx1

OPTIONS:
--option1 <value>: some description 4
   FORMAT: format3
   DEFAULT VALUE: 1

--switch1: some description 3" @>

[<Fact>]
let ``Can render the whole command description``() =
    let parameters = [|
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

    test <@ HelpCommand.commandDescription (cmdWith parameters) =
        @"somecmd: some short description

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

USAGE: somecmd <arg1> [<options>]

ARGUMENTS:
<arg1>: some description 1
   FORMAT: format1

OPTIONS:
--switch1: some description 3" @>    
