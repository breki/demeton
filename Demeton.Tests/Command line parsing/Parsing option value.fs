module Tests.``Command line parsing``.``Parsing option value``

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
let ``Returns an error if option is the last argument and its value is missing``() =
    let parameterName = "someoption"
    let remainingArgs = []
    let state = ParsingInProgress (remainingArgs, [])

    let newState = parseOptionValue doNotCallMeParser parameterName state

    test <@ newState 
            |> parsingFailed "'someoption' option's value is missing." @>


[<Fact>]
let ``Returns an error if subsequent argument after option name starts with '--'``() =
    let parameterName = "someoption"
    let remainingArgs = [ "--some-other-param"; "sdfdfs" ]
    let state = ParsingInProgress (remainingArgs, [])

    let newState = parseOptionValue doNotCallMeParser parameterName state

    test <@ newState 
            |> parsingFailed "'someoption' option's value is missing." @>
