module Tests.``Shaders``.``Building elevation coloring step``

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Demeton.Shaders.Pipeline.BuildingElevationColoring
open Png

open Xunit
open Swensen.Unquote

let isError errorDescription step =
    step = Error(
        sprintf
            "Error in step '%s': %s"
            StepNameElevationColoring
            errorDescription
    )

[<Fact>]
let ``Can parse elevation coloring step without parameters`` () =
    let parsedStep =
        { Name = StepNameElevationColoring
          Parameters = [] }

    let step = elevationColoringStepBuilder parsedStep

    test <@ step = Ok(ElevationColoring ElevationColoring.defaultParameters) @>

[<Fact>]
let ``Can parse elevation coloring step with valid parameters`` () =
    let parsedStep =
        { Name = StepNameElevationColoring
          Parameters =
            [ { Name = "scale"
                Value = "-1:#000000;2000:#ffffff;none:#000000" } ] }

    let expectedColorScale: ElevationColoring.ColorScale =
        { Marks =
            [| -1s, Rgba8Bit.rgbColor 0uy 0uy 0uy
               2000s, Rgba8Bit.rgbColor 0xffuy 0xffuy 0xffuy |]
          NoneColor = Rgba8Bit.rgbColor 0uy 0uy 0uy }

    let step = elevationColoringStepBuilder parsedStep

    test
        <@
            step = Ok(
                ElevationColoring
                    { ColorScale = expectedColorScale
                      HeightsArraysIndex = 0 }
            )
        @>

[<Fact>]
let ``Reports an error when color scale parameter is invalid`` () =
    let parsedStep =
        { Name = StepNameElevationColoring
          Parameters =
            [ { Name = "scale"
                Value = "-1:#000000;2000:#ffffff" } ] }

    let step = elevationColoringStepBuilder parsedStep

    test
        <@ step |> isError "'scale' parameter value error: invalid color scale." @>

[<Fact>]
let ``Reports an error when parameter is not recognized`` () =
    let parsedStep =
        { Name = StepNameElevationColoring
          Parameters =
            [ { Name = "somepar"
                Value = "some value" } ] }

    let step = elevationColoringStepBuilder parsedStep
    test <@ step |> isError "'somepar' parameter is not recognized." @>

[<Fact>]
let ``Can handle parsing of remaining parameters when error is found`` () =
    let parsedStep =
        { Name = StepNameElevationColoring
          Parameters =
            [ { Name = "somepar"
                Value = "some value" }
              { Name = "somepar2"
                Value = "some value" } ] }

    let step = elevationColoringStepBuilder parsedStep
    test <@ step |> isError "'somepar' parameter is not recognized." @>
