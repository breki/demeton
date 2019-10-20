module Tests.``Command line parsing``.``Parsing a sequence of parameters``

open CommandLine
open CommandLine.Common

open Xunit
open Swensen.Unquote
open TestHelp

let someArg argName = 
    Arg.build argName |> Arg.asFloat 10. |> Arg.optional |> Arg.toPar

let supportedParameters: CommandParameter[] = [|
    Switch { Name = "switch1"; Description = "" }
    Option.build "option1" |> Option.asInt |> Option.toPar
|]


[<Fact>]
let ``All command arguments need to be specified before any options and switches``() =
    let supportedParameters: CommandParameter[] = [|
        Switch { Name = "switch1"; Description = "" }
        someArg "arg1"
        Option.build "option1" |> Option.toPar
    |]
    
    let result = 
        parseParameters [] supportedParameters
    test <@ result |> isErrorData 
                "All command arguments need to be specified before any options and switches." 
        @>

[<Fact>]
let ``Reports an error if some of the command arguments are missing and there is a switch after it``() =
    let supportedParameters: CommandParameter[] = [|
        someArg "arg1"
        someArg "arg2"
        Switch { Name = "switch1"; Description = "" }
    |]
    
    let args = [ "123."; "--switch1" ]
    let result = parseParameters args supportedParameters
    test <@ result |> isErrorData 
                "<arg2> argument's value is missing." 
        @>

[<Fact>]
let ``Reports an error if some of the command arguments are missing and there are no more args``() =
    let supportedParameters: CommandParameter[] = [|
        someArg "arg1"
        someArg "arg2"
        Switch { Name = "switch1"; Description = "" }
    |]
    
    let args = [ "123." ]
    let result = parseParameters args supportedParameters
    test <@ result |> isErrorData 
                "<arg2> argument's value is missing." 
        @>

[<Fact>]
let ``Reports an error if command argument's value is invalid``() =
    let supportedParameters: CommandParameter[] = [|
        someArg "arg1"
    |]
    
    let args = [ "dsd" ]
    let result = parseParameters args supportedParameters
    test <@ result |> isErrorData 
                "<arg1> argument's value is invalid, it has to be a numeric value >= 10." 
        @>

[<Fact>]
let ``If option or switch name does not start with prefix, returns an error``() =
    let args = [ "weird" ]

    let result = 
        parseParameters args supportedParameters
    test <@ result |> isErrorData "Unrecognized parameter 'weird'." @>

[<Fact>]
let ``If option or switch is not among supported ones, returns an error``() =
    let args = [ "--par3" ]

    let result = 
        parseParameters args supportedParameters
    test <@ result |> isErrorData "Unrecognized parameter 'par3'." @>
    
[<Fact>]
let ``If parameter is a supported switch, record it in parsed parameters list``() =
    let args = [ "--switch1" ]

    let result = 
        parseParameters args supportedParameters
    test <@ result 
            |> isOkValue ([ ParsedSwitch { Name = "switch1" } ]) @>
    
[<Fact>]
let ``If parameter is a supported option, record it and its value in parsed parameters list``() =
    let args = [ "--option1"; "123" ]

    let result = 
        parseParameters args supportedParameters
    test <@ result 
            |> isOkValue ([ ParsedOption { Name = "option1"; Value = 123 } ]) @>
    
[<Fact>]
let ``Supports parsing of series of parameters``() =
    let args = [ "--switch1"; "--option1"; "123" ]

    let result = 
        parseParameters args supportedParameters
    test <@ result 
            |> isOkValue ([ 
                ParsedSwitch { Name = "switch1" }
                ParsedOption { Name = "option1"; Value = 123 } 
            ]) @>
