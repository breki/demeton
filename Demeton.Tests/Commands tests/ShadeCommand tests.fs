module ``Commands tests``.``ShadeCommand tests``

open Demeton.Commands.ShadeCommand

open Xunit
open Swensen.Unquote
open TestHelp


let parsedOptions result: ShadeOptions =
    match result with
    | Ok (_, options) -> options
    | _ -> invalidOp "Expected the parsed options."


[<Fact>]
let ``Reports error when coverage points parameter was not specified at all``() =
    let result = parseShadeArgs []
    test <@ 
            result 
            |> isError "'coverage' parameter's value is invalid, it has to have at least two points specified." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter does not have any points``() =
    let result = parseShadeArgs [ "--coverage" ]
    test <@ 
            result 
            |> isError "'coverage' parameter's value is missing." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter has a missing coordinate``() =
    let result = parseShadeArgs [ "--coverage"; "10,20,30" ]
    test <@ 
            result 
            |> isError "'coverage' parameter's value is invalid, it has an odd number of coordinates." 
            @>

[<Fact>]
let ``Accepts two coverage points``() =
    let result = parseShadeArgs [ "--coverage"; "10,20,30,40" ]
    test <@ result |> isOk @>
