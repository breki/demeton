module Tests.``Command line parsing``.``Parsing a sequence of parameters``

open CommandLine
open CommandLine.Common

open Xunit
open Swensen.Unquote
open TestHelp

let someArg argName =
    Arg.build argName |> Arg.asFloatWithMin 10. |> Arg.toPar

let someOptionalArg argName =
    Arg.build argName |> Arg.asFloatWithMin 10. |> Arg.optional |> Arg.toPar

let supportedParameters: CommandParameter[] =
    [| Switch.build "switch1" |> Switch.toPar
       Option.build "option1" |> Option.asInt |> Option.toPar |]

[<Fact>]
let ``All command arguments need to be specified before any options and switches``
    ()
    =
    let supportedParameters: CommandParameter[] =
        [| Switch.build "switch1" |> Switch.toPar
           someArg "arg1"
           Option.build "option1" |> Option.toPar |]

    let result = parseParameters [] supportedParameters

    test
        <@
            result
            |> isErrorData
                "All command arguments need to be specified before any options and switches."
        @>

[<Fact>]
let ``All mandatory command arguments need to be specified before any non-mandatory ones``
    ()
    =
    let supportedParameters: CommandParameter[] =
        [| someOptionalArg "arg1"; someArg "arg2" |]

    let result = parseParameters [] supportedParameters

    test
        <@
            result
            |> isErrorData
                "All mandatory command arguments need to be specified before any non-mandatory ones."
        @>

[<Fact>]
let ``Reports an error if some of the mandatory command arguments are missing and there is a switch after it``
    ()
    =
    let supportedParameters: CommandParameter[] =
        [| someArg "arg1"
           someArg "arg2"
           Switch.build "switch1" |> Switch.toPar |]

    let args = [ "123."; "--switch1" ]
    let result = parseParameters args supportedParameters
    test <@ result |> isErrorData "<arg2> argument's value is missing." @>

[<Fact>]
let ``Reports an error if some of the mandatory command arguments are missing and there are no more args``
    ()
    =
    let supportedParameters: CommandParameter[] =
        [| someArg "arg1"
           someArg "arg2"
           Switch.build "switch1" |> Switch.toPar |]

    let args = [ "123." ]
    let result = parseParameters args supportedParameters
    test <@ result |> isErrorData "<arg2> argument's value is missing." @>

[<Fact>]
let ``Allows an optional command argument to not be specified (case when there are no parameters left)``
    ()
    =
    let supportedParameters: CommandParameter[] =
        [| someArg "arg1"; someOptionalArg "arg2" |]

    let args = [ "123" ]

    let result = parseParameters args supportedParameters

    test <@ result |> isOkValue [ ParsedArg { Name = "arg1"; Value = 123. } ] @>

[<Fact>]
let ``Allows an optional command argument to not be specified (case when there follows an option)``
    ()
    =
    let supportedParameters: CommandParameter[] =
        [| someArg "arg1"
           someOptionalArg "arg2"
           Switch.build "switch1" |> Switch.toPar |]

    let args = [ "123"; "--switch1" ]

    let result = parseParameters args supportedParameters

    test
        <@
            result
            |> isOkValue
                [ ParsedArg { Name = "arg1"; Value = 123. }
                  ParsedSwitch { Name = "switch1" } ]
        @>

[<Fact>]
let ``Reports an error if command argument's value is invalid`` () =
    let supportedParameters: CommandParameter[] = [| someArg "arg1" |]

    let args = [ "dsd" ]
    let result = parseParameters args supportedParameters

    test
        <@
            result
            |> isErrorData
                "<arg1> argument's value is invalid, it has to be a numeric value >= 10."
        @>

[<Fact>]
let ``If option or switch name does not start with prefix, returns an error``
    ()
    =
    let args = [ "weird" ]

    let result = parseParameters args supportedParameters
    test <@ result |> isErrorData "Unrecognized parameter 'weird'." @>

[<Fact>]
let ``If option or switch is not among supported ones, returns an error`` () =
    let args = [ "--par3" ]

    let result = parseParameters args supportedParameters
    test <@ result |> isErrorData "Unrecognized parameter 'par3'." @>

[<Fact>]
let ``If parameter is a supported switch, record it in parsed parameters list``
    ()
    =
    let args = [ "--switch1" ]

    let result = parseParameters args supportedParameters
    test <@ result |> isOkValue [ ParsedSwitch { Name = "switch1" } ] @>

[<Fact>]
let ``If parameter is a supported option, record it and its value in parsed parameters list``
    ()
    =
    let args = [ "--option1"; "123" ]

    let result = parseParameters args supportedParameters

    test
        <@
            result
            |> isOkValue [ ParsedOption { Name = "option1"; Value = 123 } ]
        @>

[<Fact>]
let ``Supports parsing of series of parameters (case 1)`` () =
    let pars =
        [| someArg "arg1"
           someOptionalArg "arg2"
           Switch.build "switch1" |> Switch.toPar
           Option.build "option1" |> Option.asInt |> Option.toPar |]

    let args = [ "123"; "234"; "--switch1"; "--option1"; "123" ]

    let result = parseParameters args pars

    test
        <@
            result
            |> isOkValue
                [ ParsedArg { Name = "arg1"; Value = 123. }
                  ParsedArg { Name = "arg2"; Value = 234. }
                  ParsedSwitch { Name = "switch1" }
                  ParsedOption { Name = "option1"; Value = 123 } ]
        @>

[<Fact>]
let ``Supports parsing of series of parameters (case 2)`` () =
    let args = [ "--switch1"; "--option1"; "123" ]

    let result = parseParameters args supportedParameters

    test
        <@
            result
            |> isOkValue
                [ ParsedSwitch { Name = "switch1" }
                  ParsedOption { Name = "option1"; Value = 123 } ]
        @>
