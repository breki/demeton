module ``Commands tests``.``ShadeCommand``.``Command line parsing``

open CommandLine.Common
open Demeton.Commands
open Demeton.Shaders.Pipeline.Common

open Xunit
open Swensen.Unquote
open TestHelp


let parseArgs args = 
    let result = 
        parseParameters args ShadeCommand.supportedParameters
    match result with
    | Ok parsedParameters -> 
        parsedParameters |> ShadeCommand.fillOptions |> Ok
    | Error error -> Error error


let isOkWithOptions result: ShadeCommand.Options =
    match result with
    | Ok options -> options
    | _ -> invalidOp "Expected the parsed options."

[<Fact>]
let ``Sane defaults are used for options``() =
    let result = parseArgs [ "10,20,30,40" ]
    let options = isOkWithOptions result

    test <@ options.MapScale.Dpi = 300. @>
    test <@ options.FilePrefix = "shading" @>
    test <@ options.MapScale.MapScale = 50000. @>
    test <@ options.OutputDir = "output" @>
    // todo
    //test <@ 
    //        options.Shader 
    //            = ElevationColoringShader elevationColorScaleMaperitive @>

[<Fact>]
let ``Reports error when coverage points parameter does not have any points``() =
    let result = parseArgs [ ]
    test <@ 
            result 
            |> isErrorData "<coverage> argument's value is missing." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter has an invalid value``() =
    let result = parseArgs [ "10,a,30,40" ]
    test <@ 
            result 
            |> isErrorData "<coverage> argument's value is invalid, it has to consist of a list of coordinates." 
            @>

[<Fact>]
let ``Reports error when coverage points parameter has a missing coordinate``() =
    let result = parseArgs [ "10,20,30" ]
    test <@ 
            result 
            |> isErrorData "<coverage> argument's value is invalid, it has an odd number of coordinates." 
            @>

[<Fact>]
let ``Reports error when there are less than 2 coverage points``() =
    let result = parseArgs [ "10,20" ]
    test <@ 
            result 
            |> isErrorData "<coverage> argument's value is invalid, it has to have at least two points specified." 
            @>

[<Fact>]
let ``Accepts two coverage points and places them into the options``() =
    let result = parseArgs [ "10,20,30,40" ]
    test <@ result |> isOk @>
    test <@ 
            (isOkWithOptions result).CoveragePoints 
                = [ (10., 20.); (30., 40.) ] 
        @>

[<Fact>]
let ``Map scale has to be a numeric value`` () =
    let result = 
        parseArgs [ "10,20,30,40"; "--map-scale"; "xyz" ]
    test <@ 
            result 
            |> isErrorData "'map-scale' option's value is invalid, it has to be a numeric value >= 1." 
            @>

[<Theory>]
[<InlineData("-10")>]
[<InlineData("0")>]
[<InlineData("0.1")>]
let ``Map scale has to be a positive value larger than 1`` mapScaleString =
    let result = 
        parseArgs [ 
            "10,20,30,40"; "--map-scale"; mapScaleString ]
    test <@ 
            result 
            |> isErrorData "'map-scale' option's value is invalid, it has to be a numeric value >= 1." 
            @>

[<Fact>]
let ``Accepts a valid map scale value and puts it into the options`` () =
    let result = 
        parseArgs [ "10,20,30,40"; "--map-scale"; "100000" ]
    test <@ result |> isOk @>
    test <@ 
            (isOkWithOptions result).MapScale.MapScale = 100000. 
        @>

[<Fact>]
let ``DPI has to be a numeric value`` () =
    let result = 
        parseArgs [ "10,20,30,40"; "--dpi"; "xyz" ]
    test <@ 
            result 
            |> isErrorData "'dpi' option's value is invalid, it has to be a positive numeric value." 
            @>

[<Theory>]
[<InlineData("-10")>]
[<InlineData("0")>]
let ``DPI has to be a positive value`` mapScaleString =
    let result = 
        parseArgs [ "10,20,30,40"; "--dpi"; mapScaleString ]
    test <@ 
            result 
            |> isErrorData "'dpi' option's value is invalid, it has to be a positive numeric value." 
            @>

[<Fact>]
let ``Accepts a valid DPI value and puts it into the options`` () =
    let result = 
        parseArgs [ "10,20,30,40"; "--dpi"; "72" ]
    test <@ result |> isOk @>
    test <@ 
            (isOkWithOptions result).MapScale.Dpi = 72. 
        @>

[<Fact>]
let ``Tile size has to be a numeric value`` () =
    let result = 
        parseArgs [ "10,20,30,40"; "--tile-size"; "xyz" ]
    test <@ 
            result 
            |> isErrorData "'tile-size' option's value is invalid, it has to be an integer value larger than 0." 
            @>

[<Theory>]
[<InlineData("-10")>]
[<InlineData("0")>]
let ``Tile size has to be a positive value`` tileSizeString =
    let result = 
        parseArgs [ "10,20,30,40"; "--tile-size"; tileSizeString ]
    test <@ 
            result 
            |> isErrorData "'tile-size' option's value is invalid, it has to be an integer value larger than 0." 
            @>

[<Fact>]
let ``Accepts a valid tile size value and puts it into the options`` () =
    let result = 
        parseArgs [ "10,20,30,40"; "--tile-size"; "3000" ]
    test <@ result |> isOk @>
    test <@ 
            (isOkWithOptions result).TileSize = 3000
        @>

[<Fact>]
let ``FileName has to be a valid file name`` () =
    let result = 
        parseArgs [ 
            "10,20,30,40"; "--file-prefix"; Pth.combine "test" "some" ]
    test <@ 
            result 
            |> isErrorData "'file-prefix' option's value is invalid, it has to consist of valid path characters." 
            @>

[<Fact>]
let ``Accepts a valid FileName value and puts it into the options`` () =
    let result = 
        parseArgs [ "10,20,30,40"; "--file-prefix"; "hillshading" ]
    test <@ result |> isOk @>
    test <@ 
            (isOkWithOptions result).FilePrefix = "hillshading" 
        @>

[<Fact>]
let ``Accepts a valid OutputDir value and puts it into the options`` () =
    let result = 
        parseArgs [ 
            "10,20,30,40"; "--output-dir"; "some/hillshading" ]
    test <@ result |> isOk @>
    test <@ 
            (isOkWithOptions result).OutputDir = "some/hillshading" 
        @>

[<Fact>]
let ``Accepts a valid shading script value and puts it into the options`` () =
    let result = 
        parseArgs [ 
            "10,20,30,40"; "--shading-script"; "elecolor" ]
    test <@ result |> isOk @>
    test <@ match (isOkWithOptions result).RootShadingStep with 
            | ElevationColoring _ -> true
            | _ -> false
        @>

[<Fact>]
let ``Reports an error when the shading script is empty``() =
    let result = 
        parseArgs [ 
            "10,20,30,40"; "--shading-script"; "" ]
    test <@ result
            |> isErrorData 
                @"'shading-script' option's value is invalid:
Shading pipeline is empty."
        @>

[<Fact>]
let ``Reports an error when the shading script has a syntax error``() =
    let result = 
        parseArgs [ 
            "10,20,30,40"; "--shading-script"; "sasd)" ]
    test <@ result
            |> isErrorData 
                @"'shading-script' option's value is invalid:
sasd)
    ^
Expected: step operator, step parameters."
        @>
    
[<Fact>]
let ``Reports an error when the shading script has an unrecognized step``() =
    let result = 
        parseArgs [ 
            "10,20,30,40"; "--shading-script"; "something" ]
    test <@ result
            |> isErrorData 
                @"'shading-script' option's value is invalid:
Unrecognized shading step 'something'."
        @>
    
[<Fact>]
let ``Reports an error when the shading script step has an error``() =
    let result = 
        parseArgs [ 
            "10,20,30,40"; "--shading-script"; "elecolor(scale='sdsd')" ]
    test <@ result
            |> isErrorData 
                @"'shading-script' option's value is invalid:
Error in step 'elecolor': 'scale' parameter value error: invalid color scale."
        @>
