module Tests.``Command line parsing``.``Parsing parameters``

open Demeton.CommandLineParsing
open Xunit
open Swensen.Unquote    
open TestHelp

let doNotCallMeParser _ _ 
    = invalidOp "This function shouldn't have been called."

[<Fact>]
let ``Returns an error if parameter is the last argument and its value is missing``() =
    let parameterName = "someparam"
    let remainingArgs = []
    let options = ""
    let context = (remainingArgs, options)

    let result = parseParameterValue doNotCallMeParser parameterName context

    test <@ result |> isErrorData "'someparam' parameter's value is missing." @>


[<Fact>]
let ``Returns an error if subsequent argument after parameter name starts with '--'``() =
    let parameterName = "someparam"
    let remainingArgs = [ "--some-other-param"; "sdfdfs" ]
    let options = ""
    let context = (remainingArgs, options)

    let result = parseParameterValue doNotCallMeParser parameterName context

    test <@ result |> isErrorData "'someparam' parameter's value is missing." @>
