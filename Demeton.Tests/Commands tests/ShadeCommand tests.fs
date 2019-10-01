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
            |> isErrorData "'coverage' parameter's value is invalid, it has to have at least two points specified." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter does not have any points``() =
    let result = parseShadeArgs [ "--coverage" ]
    test <@ 
            result 
            |> isErrorData "'coverage' parameter's value is missing." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter has an invalid value``() =
    let result = parseShadeArgs [ "--coverage"; "10,a,30,40" ]
    test <@ 
            result 
            |> isErrorData "'coverage' parameter's value is invalid, it has to consist of a list of coordinates." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter has a missing coordinate``() =
    let result = parseShadeArgs [ "--coverage"; "10,20,30" ]
    test <@ 
            result 
            |> isErrorData "'coverage' parameter's value is invalid, it has an odd number of coordinates." 
            @>

[<Fact>]
let ``Reports error when there are less than 2 coverage points``() =
    let result = parseShadeArgs [ "--coverage"; "10,20" ]
    test <@ 
            result 
            |> isErrorData "'coverage' parameter's value is invalid, it has to have at least two points specified." 
            @>

[<Fact>]
let ``Accepts two coverage points and places them into the options``() =
    let result = parseShadeArgs [ "--coverage"; "10,20,30,40" ]
    test <@ result |> isOk @>
    test <@ 
            (parsedOptions result).CoveragePoints 
                = [ (10., 20.); (30., 40.) ] 
        @>

[<Theory>]
[<InlineData("-10")>]
let ``Map scale has to be a positive value larger than 1`` mapScaleString =
    let result = 
        parseShadeArgs [ 
            "--coverage"; "10,20,30,40"; "--map-scale"; mapScaleString ]
    test <@ 
            result 
            |> isErrorData "'map-scale' parameter's value is invalid, it has to be a value larger than 1." 
            @>
    