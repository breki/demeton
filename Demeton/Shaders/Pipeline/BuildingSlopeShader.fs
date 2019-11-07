module Demeton.Shaders.Pipeline.BuildingSlopeShader

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Building
open Png

[<Literal>]
let StepNameSlopeShading = "slope"

let slopeShaderStepBuilder: ShadingStepBuildingFunc = fun parsedStep ->
    let parseHorizontalColor value (settings: SlopeShader.ShaderParameters) =
        match Rgba8Bit.tryParseColorHexValue value with
        | Ok color -> Ok { settings with HorizontalColor = color }
        | Error _ -> Result.Error "invalid color value"

    let parseVerticalColor value (settings: SlopeShader.ShaderParameters) =
        match Rgba8Bit.tryParseColorHexValue value with
        | Ok color -> Ok { settings with VerticalColor = color }
        | Error _ -> Result.Error "invalid color value"

    let parDefs = dict [
        "hcol", parseHorizontalColor
        "vcol", parseVerticalColor
    ]

    let stepBuildingResult = 
        stepBuilder parsedStep parDefs SlopeShader.defaultParameters
    match stepBuildingResult with
    | Ok finalSettings -> SlopeShading finalSettings |> Ok
    | Error errorMessage -> Result.Error errorMessage
