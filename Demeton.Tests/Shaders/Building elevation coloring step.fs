module Tests.``Shaders``.``Building elevation coloring step``

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Demeton.Shaders.Pipeline.BuildingElevationColoring
open Png

open Xunit
open Swensen.Unquote


[<Fact>]
let ``Can parse elevation coloring step without parameters``() =
    let parsedStep = { Name = "elecolor"; Parameters = [] } 

    let step = elevationColoringStepBuilder parsedStep
    test
        <@ step =
            Ok
                (ElevationColoring
                    ({ ColorScale = ElevationColoring.colorScaleMaperitive })) 
        @>

[<Fact>]
let ``Can parse elevation coloring step with valid parameters``() =
    let parsedStep = 
        { Name = "elecolor"; 
        Parameters = 
            [ { Name = "scale"; 
                Value = "-1:#000000;2000:#ffffff;none:#000000" } ] } 

    let expectedColorScale: ElevationColoring.ColorScale = 
        {
            Marks = [| 
                -1s, Rgba8Bit.rgbColor 0uy 0uy 0uy 
                2000s, Rgba8Bit.rgbColor 0xffuy 0xffuy 0xffuy 
                |]
            NoneColor = Rgba8Bit.rgbColor 0uy 0uy 0uy
        }

    let step = elevationColoringStepBuilder parsedStep
    test <@ step = Ok (ElevationColoring { ColorScale = expectedColorScale }) 
        @>

[<Fact>]
let ``Reports an error when color scale parameter is invalid``() =
    let parsedStep = 
        { Name = "elecolor"; 
        Parameters = 
            [ { Name = "scale"; Value = "-1:#000000;2000:#ffffff" } ] } 

    let step = elevationColoringStepBuilder parsedStep
    test <@ step = 
        Error 
            ("Error in step 'elecolor': 'scale' parameter value error: " 
                + "invalid color scale.") @>
    
[<Fact>]
let ``Reports an error when parameter is not recognized``() =
    let parsedStep = 
        { Name = "elecolor"; 
        Parameters = 
            [ { Name = "somepar"; Value = "some value" } ] } 

    let step = elevationColoringStepBuilder parsedStep
    test <@ step = 
        Error 
            "Error in step 'elecolor': 'somepar' parameter is not recognized." 
            @>
    
[<Fact>]
let ``Can handle parsing of remaining parameters when error is found``() =
    let parsedStep = 
        { Name = "elecolor"; 
        Parameters = 
            [ { Name = "somepar"; Value = "some value" };
            { Name = "somepar2"; Value = "some value" } ] } 

    let step = elevationColoringStepBuilder parsedStep
    test <@ step = 
        Error 
            "Error in step 'elecolor': 'somepar' parameter is not recognized." 
            @>
