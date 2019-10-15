module Tests.``Command line parsing``.``Parsing a sequence of parameters``

open CommandLine.Common

open Xunit
open Swensen.Unquote
open TestHelp


let supportedParameters: CommandParameter[] = [|
    Switch { Name = "switch1" }
    Option { Name = "option1"; Parser = parseIntOptionValue }
|]

[<Fact>]
let ``If parameter name does not start with prefix, returns an error``() =
    let args = [ "weird" ]

    let result = 
        parseParameters args supportedParameters
    test <@ result |> isErrorData "Unrecognized parameter 'weird'." @>

[<Fact>]
let ``If parameter is not among supported ones, returns an error``() =
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
