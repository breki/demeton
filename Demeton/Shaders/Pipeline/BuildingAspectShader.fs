module Demeton.Shaders.Pipeline.BuildingAspectShader

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Building
open Png

let aspectShaderStepBuilder: ShadingStepBuildingFunc = fun parsedStep ->
    let parseNorthColor value (settings: AspectShader.ShaderParameters) =
        match Rgba8Bit.tryParseColorHexValue value with
        | Ok color -> Ok { settings with NorthColor = color }
        | Error _ -> Result.Error "invalid color value"

    let parseEastColor value (settings: AspectShader.ShaderParameters) =
        match Rgba8Bit.tryParseColorHexValue value with
        | Ok color -> Ok { settings with EastColor = color }
        | Error _ -> Result.Error "invalid color value"

    let parseSouthColor value (settings: AspectShader.ShaderParameters) =
        match Rgba8Bit.tryParseColorHexValue value with
        | Ok color -> Ok { settings with SouthColor = color }
        | Error _ -> Result.Error "invalid color value"

    let parseWestColor value (settings: AspectShader.ShaderParameters) =
        match Rgba8Bit.tryParseColorHexValue value with
        | Ok color -> Ok { settings with WestColor = color }
        | Error _ -> Result.Error "invalid color value"

    let parDefs = dict [
        "ncol", parseNorthColor
        "ecol", parseEastColor
        "scol", parseSouthColor
        "wcol", parseWestColor
    ]

    let stepBuildingResult = 
        stepBuilder parsedStep parDefs AspectShader.defaultParameters
    match stepBuildingResult with
    | Ok finalSettings -> AspectShading finalSettings |> Ok
    | Error errorMessage -> Result.Error errorMessage
