module Tests.``Command line parsing``.``Parsing parameter value``

open CommandLine.Common
open Xunit
open Swensen.Unquote    

let doNotCallMeParser _ 
    = invalidOp "This function shouldn't have been called."

let parsingFailed expectedReason state =
    match state with
    | ParsingFail reason -> reason = expectedReason
    | _ -> false

[<Fact>]
let ``Returns an error if parameter is the last argument and its value is missing``() =
    let parameterName = "someparam"
    let remainingArgs = []
    let state = ParsingInProgress (remainingArgs, [])

    let newState = parseOptionValue doNotCallMeParser parameterName state

    test <@ newState 
            |> parsingFailed "'someparam' parameter's value is missing." @>


[<Fact>]
let ``Returns an error if subsequent argument after parameter name starts with '--'``() =
    let parameterName = "someparam"
    let remainingArgs = [ "--some-other-param"; "sdfdfs" ]
    let state = ParsingInProgress (remainingArgs, [])

    let newState = parseOptionValue doNotCallMeParser parameterName state

    test <@ newState 
            |> parsingFailed "'someparam' parameter's value is missing." @>
