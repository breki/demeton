module ``Commands tests``.``Command line parameters parsing``

open CommandLine

open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
let ``Parsing a list of floats``() =

    let input = "2.22,2.3"
    let result = TextParsers.parseFloatsList input
    test <@ result |> isOk @>
    test <@ result |> isOkValue [ 2.22; 2.3 ] @>

