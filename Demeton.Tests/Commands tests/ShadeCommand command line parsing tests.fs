﻿module ``Commands tests``.``ShadeCommand command line parsing tests``

open Demeton.Commands

open Xunit
open Swensen.Unquote
open TestHelp


let parsedOptions result: ShadeCommand.Options =
    match result with
    | Ok (_, options) -> options
    | _ -> invalidOp "Expected the parsed options."

[<Fact>]
let ``Sane defaults are used for options``() =
    let result = ShadeCommand.parseArgs [ "--coverage"; "10,20,30,40" ]
    let options = parsedOptions result

    test <@ options.Dpi = 300. @>
    test <@ options.FileName = "shading" @>
    test <@ options.MapScale = 50000. @>
    test <@ options.OutputDir = "output" @>

[<Fact>]
let ``Reports error when coverage points parameter was not specified at all``() =
    let result = ShadeCommand.parseArgs []
    test <@ 
            result 
            |> isErrorData "'coverage' parameter's value is invalid, it has to have at least two points specified." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter does not have any points``() =
    let result = ShadeCommand.parseArgs [ "--coverage" ]
    test <@ 
            result 
            |> isErrorData "'coverage' parameter's value is missing." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter has an invalid value``() =
    let result = ShadeCommand.parseArgs [ "--coverage"; "10,a,30,40" ]
    test <@ 
            result 
            |> isErrorData "'coverage' parameter's value is invalid, it has to consist of a list of coordinates." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter has a missing coordinate``() =
    let result = ShadeCommand.parseArgs [ "--coverage"; "10,20,30" ]
    test <@ 
            result 
            |> isErrorData "'coverage' parameter's value is invalid, it has an odd number of coordinates." 
            @>

[<Fact>]
let ``Reports error when there are less than 2 coverage points``() =
    let result = ShadeCommand.parseArgs [ "--coverage"; "10,20" ]
    test <@ 
            result 
            |> isErrorData "'coverage' parameter's value is invalid, it has to have at least two points specified." 
            @>

[<Fact>]
let ``Accepts two coverage points and places them into the options``() =
    let result = ShadeCommand.parseArgs [ "--coverage"; "10,20,30,40" ]
    test <@ result |> isOk @>
    test <@ 
            (parsedOptions result).CoveragePoints 
                = [ (10., 20.); (30., 40.) ] 
        @>

[<Fact>]
let ``Map scale has to be a numeric value`` () =
    let result = 
        ShadeCommand.parseArgs [ 
            "--coverage"; "10,20,30,40"; "--map-scale"; "xyz" ]
    test <@ 
            result 
            |> isErrorData "'map-scale' parameter's value is invalid, it has to be a numeric value larger than 1." 
            @>

[<Theory>]
[<InlineData("-10")>]
[<InlineData("0")>]
[<InlineData("0.1")>]
let ``Map scale has to be a positive value larger than 1`` mapScaleString =
    let result = 
        ShadeCommand.parseArgs [ 
            "--coverage"; "10,20,30,40"; "--map-scale"; mapScaleString ]
    test <@ 
            result 
            |> isErrorData "'map-scale' parameter's value is invalid, it has to be a value larger than 1." 
            @>

[<Fact>]
let ``Accepts a valid map scale value and puts it into the options`` () =
    let result = 
        ShadeCommand.parseArgs [ 
            "--coverage"; "10,20,30,40"; "--map-scale"; "100000" ]
    test <@ result |> isOk @>
    test <@ 
            (parsedOptions result).MapScale = 100000. 
        @>

[<Fact>]
let ``DPI has to be a numeric value`` () =
    let result = 
        ShadeCommand.parseArgs [ 
            "--coverage"; "10,20,30,40"; "--dpi"; "xyz" ]
    test <@ 
            result 
            |> isErrorData "'dpi' parameter's value is invalid, it has to be a positive numeric value." 
            @>

[<Theory>]
[<InlineData("-10")>]
[<InlineData("0")>]
let ``DPI has to be a positive value`` mapScaleString =
    let result = 
        ShadeCommand.parseArgs [ 
            "--coverage"; "10,20,30,40"; "--dpi"; mapScaleString ]
    test <@ 
            result 
            |> isErrorData "'dpi' parameter's value is invalid, it has to be a positive numeric value." 
            @>

[<Fact>]
let ``Accepts a valid DPI value and puts it into the options`` () =
    let result = 
        ShadeCommand.parseArgs [ 
            "--coverage"; "10,20,30,40"; "--dpi"; "72" ]
    test <@ result |> isOk @>
    test <@ 
            (parsedOptions result).Dpi = 72. 
        @>

[<Fact>]
let ``FileName has to be a valid file name`` () =
    let result = 
        ShadeCommand.parseArgs [ 
            "--coverage"; "10,20,30,40"; "--file-name"; Pth.combine "test" "some" ]
    test <@ 
            result 
            |> isErrorData "'file-name' parameter's value is invalid, it has to consist of valid path characters." 
            @>

[<Fact>]
let ``Accepts a valid FileName value and puts it into the options`` () =
    let result = 
        ShadeCommand.parseArgs [ 
            "--coverage"; "10,20,30,40"; "--file-name"; "hillshading" ]
    test <@ result |> isOk @>
    test <@ 
            (parsedOptions result).FileName = "hillshading" 
        @>

[<Fact>]
let ``Accepts a valid OutputDir value and puts it into the options`` () =
    let result = 
        ShadeCommand.parseArgs [ 
            "--coverage"; "10,20,30,40"; "--output-dir"; "some/hillshading" ]
    test <@ result |> isOk @>
    test <@ 
            (parsedOptions result).OutputDir = "some/hillshading" 
        @>