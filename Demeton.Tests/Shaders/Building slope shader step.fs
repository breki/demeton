module Tests.Shaders.``Building slope shader step``

open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Demeton.Shaders.Pipeline.BuildingSlopeShader
open Png

open Xunit
open Swensen.Unquote

let isError errorDescription step =
    step = Error 
            (sprintf 
                "Error in step '%s': %s" 
                StepNameSlopeShading 
                errorDescription)

[<Fact>]
let ``Can parse step without parameters``() =
    let parsedStep = { Name = StepNameSlopeShading; Parameters = [] } 

    let step = slopeShaderStepBuilder parsedStep
    test
        <@ step =
            Ok (SlopeShading
                 ({ HorizontalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy; 
                    VerticalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 255uy })) 
        @>

[<Fact>]
let ``Can parse step with valid parameters``() =
    let parsedStep = 
        { Name = StepNameSlopeShading; 
        Parameters =
            [ { Name = "hcol"; Value = "#00000000" };
                { Name = "vcol"; Value = "#333333" } ] } 

    let step = slopeShaderStepBuilder parsedStep
    test
        <@ step =
            Ok (SlopeShading
                 ({ HorizontalColor = 0u; 
                    VerticalColor = 
                        Rgba8Bit.rgbaColor 0x33uy 0x33uy 0x33uy 0xffuy })) 
        @>

[<Fact>]
let ``Reports an error when horizontal color is invalid``() =
    let parsedStep = 
        { Name = StepNameSlopeShading; 
        Parameters =
            [ { Name = "hcol"; Value = "2234" };
                { Name = "vcol"; Value = "#333333" } ] } 

    let step = slopeShaderStepBuilder parsedStep
    test <@ step
            |> isError "'hcol' parameter value error: invalid color value." @>

[<Fact>]
let ``Reports an error when vertical color is invalid``() =
    let parsedStep = 
        { Name = StepNameSlopeShading; 
        Parameters =
            [ { Name = "hcol"; Value = "#333333" };
                { Name = "vcol"; Value = "2234" } ] } 

    let step = slopeShaderStepBuilder parsedStep
    test <@ step
            |> isError "'vcol' parameter value error: invalid color value." @>
