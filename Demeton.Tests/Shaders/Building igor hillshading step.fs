module Tests.Shaders.``Building igor hillshading step``

open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Demeton.Shaders.Pipeline.BuildingIgorHillshading
open Demeton.Geometry.Common

open Xunit
open Swensen.Unquote

let isError errorDescription step =
    step = Error
        $"Error in step '%s{StepNameIgorHillshading}': %s{errorDescription}"

[<Fact>]
let ``Can parse step without parameters`` () =
    let parsedStep =
        { Name = StepNameIgorHillshading
          Parameters = [] }

    let step = igorHillshadingStepBuilder parsedStep

    test
        <@
            step = Ok(
                IgorHillshading(
                    { SunAzimuth = degToRad -45.
                      ShadingColor = 0u
                      Intensity = 1.
                      HeightsArrayIndex = 0 }
                )
            )
        @>

[<Fact>]
let ``Can parse step with valid parameters`` () =
    let parsedStep =
        { Name = StepNameIgorHillshading
          Parameters =
            [ { Name = "sunaz"; Value = "-90" }
              { Name = "shadcol"; Value = "#333333" } ] }

    let step = igorHillshadingStepBuilder parsedStep

    test
        <@
            step = Ok(
                IgorHillshading(
                    { SunAzimuth = degToRad -90.
                      ShadingColor = 0x333333ffu
                      Intensity = 1.
                      HeightsArrayIndex = 0 }
                )
            )
        @>

[<Fact>]
let ``Reports an error when shading color is invalid`` () =
    let parsedStep =
        { Name = StepNameIgorHillshading
          Parameters =
            [ { Name = "sunaz"; Value = "-90" }
              { Name = "shadcol"; Value = "wsdd" } ] }

    let step = igorHillshadingStepBuilder parsedStep

    test
        <@
            step
            |> isError "'shadcol' parameter value error: invalid color value."
        @>

[<Fact>]
let ``Reports an error when sun azimuth is invalid`` () =
    let parsedStep =
        { Name = StepNameIgorHillshading
          Parameters =
            [ { Name = "sunaz"; Value = "xcd" }
              { Name = "shadcol"; Value = "#333333" } ] }

    let step = igorHillshadingStepBuilder parsedStep

    test
        <@
            step
            |> isError "'sunaz' parameter value error: invalid degrees value."
        @>
