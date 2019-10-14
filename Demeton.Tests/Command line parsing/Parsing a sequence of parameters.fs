module Tests.``Command line parsing``.``Parsing a sequence of parameters``

open Demeton.CommandLineParsing

open Xunit
open Swensen.Unquote
open TestHelp

type TestOptions = { Value: int }

let par1Parser name context: ParsingResult<TestOptions> = 
    let (_, oldOptions) = context

    Ok (context 
        |> withOptions { oldOptions with Value = oldOptions.Value + 1 })

let par2Parser name context: ParsingResult<TestOptions> = 
    let (_, oldOptions) = context

    Ok (context 
        |> withOptions { oldOptions with Value = oldOptions.Value + 2 })

let supportedParameters: CommandLineParameter<TestOptions>[] = [|
    { Name = "par1"; Parser = par1Parser }
    { Name = "par2"; Parser = par2Parser }
|]

let defaultOptions = { Value = 0 }

let optionsAreOk finalOptions = finalOptions |> finalOkResult

[<Fact>]
let ``If parameter name does not start with prefix, returns an error``() =
    let args = [ "weird" ]

    let result = 
        parseParameters optionsAreOk args supportedParameters defaultOptions
    test <@ result |> isErrorData "Unrecognized parameter 'weird'." @>

[<Fact>]
let ``If parameter is not among supported ones, returns an error``() =
    let args = [ "--par3" ]

    let result = 
        parseParameters optionsAreOk args supportedParameters defaultOptions
    test <@ result |> isErrorData "Unrecognized parameter 'par3'." @>
    
[<Fact>]
let ``If parameter is supported, calls its parser``() =
    let args = [ "--par1" ]

    let result = 
        parseParameters optionsAreOk args supportedParameters defaultOptions
    test <@ result |> isOkValue ([], { Value = 1 }) @>
    
[<Fact>]
let ``Supports parsing of series of parameters``() =
    let args = [ "--par1"; "--par2" ]

    let result = 
        parseParameters optionsAreOk args supportedParameters defaultOptions
    test <@ result |> isOkValue ([], { Value = 3 }) @>
    
[<Fact>]
let ``After successfully parsing the parameters, calls the options validator which says it's OK``() =
    let args = [ "--par1"; "--par2" ]
    
    let mutable validatorWasCalled = false

    let optionsValidator options =
        validatorWasCalled <- true
        match options = { Value = 3 } with
        | true -> finalOkResult options
        | false -> 
            options 
            |> finalContext 
            |> withError "the received options are wrong"

    let result = 
        parseParameters 
            optionsValidator
            args 
            supportedParameters 
            defaultOptions

    test <@ validatorWasCalled @>
    test <@ result |> isOkValue ([], { Value = 3 }) @>

