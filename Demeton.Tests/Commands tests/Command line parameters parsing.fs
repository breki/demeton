module ``Commands tests``.``Command line parameters parsing``

open Commands.ParametersParsing

open Xunit

[<Fact>]
let ``Parsing a list of floats``() =

    let input = "2.22,2.3"
    let result = parseFloatsList input
    printfn "result: %A" result
