module Tests.Shaders.``Building aspect shader step``

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Demeton.Shaders.Pipeline.BuildingAspectShader
open Demeton.Shaders.Types

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Can parse step without parameters`` () =
    let parsedStep =
        { Name = StepNameAspectShading
          Parameters = [] }

    let step = aspectShaderStepBuilder parsedStep
    test <@ step = Ok(AspectShading AspectShader.defaultParameters) @>

[<Fact>]
let ``Can parse step with valid parameters`` () =
    let parsedStep =
        { Name = StepNameAspectShading
          Parameters =
            [ { Name = "ncol"; Value = "#000000" }
              { Name = "ecol"; Value = "#111111" }
              { Name = "scol"; Value = "#222222" }
              { Name = "wcol"; Value = "#333333" } ] }

    let step = aspectShaderStepBuilder parsedStep

    test
        <@
            step = Ok(
                AspectShading(
                    { NorthColor = 0x000000ffu
                      EastColor = 0x111111ffu
                      SouthColor = 0x222222ffu
                      WestColor = 0x333333ffu
                      DataSourceKey = DefaultDataSourceKey }
                )
            )
        @>

[<Theory>]
[<InlineData("ncol", "sdsds", "#123456", "#123456", "#123456")>]
[<InlineData("ecol", "#123456", "sdsds", "#123456", "#123456")>]
[<InlineData("scol", "#123456", "#123456", "sdsds", "#123456")>]
[<InlineData("wcol", "#123456", "#123456", "#123456", "sdsds")>]
let ``Reports an error a color value is invalid``
    (
        colorName,
        nColor,
        eColor,
        sColor,
        wColor
    ) =
    let parsedStep =
        { Name = StepNameAspectShading
          Parameters =
            [ { Name = "ncol"; Value = nColor }
              { Name = "ecol"; Value = eColor }
              { Name = "scol"; Value = sColor }
              { Name = "wcol"; Value = wColor } ] }

    let step = aspectShaderStepBuilder parsedStep

    test
        <@
            step = Result.Error(
                sprintf
                    "Error in step '%s': '%s' parameter value error: invalid color value."
                    StepNameAspectShading
                    colorName
            )
        @>
