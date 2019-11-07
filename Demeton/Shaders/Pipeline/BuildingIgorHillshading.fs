module Demeton.Shaders.Pipeline.BuildingIgorHillshading

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Building
open Demeton.Geometry.Common
open Png

[<Literal>]
let StepNameIgorHillshading = "igor"

let igorHillshadingStepBuilder: ShadingStepBuildingFunc = fun parsedStep ->
    let parseSunAzimuth value (settings: IgorHillshader.ShaderParameters) =
        match TextParsers.parseFloat value with
        | Ok degrees -> Ok { settings with SunAzimuth = degToRad degrees }
        | Error _ -> Result.Error "invalid degrees value"

    let parseShadingColor value (settings: IgorHillshader.ShaderParameters) =
        match Rgba8Bit.tryParseColorHexValue value with
        | Ok color -> Ok { settings with ShadingColor = color }
        | Error _ -> Result.Error "invalid color value"

    let parDefs = dict [
        "sunaz", parseSunAzimuth
        "shadcol", parseShadingColor
    ]

    let stepBuildingResult = 
        stepBuilder parsedStep parDefs IgorHillshader.defaultParameters
    match stepBuildingResult with
    | Ok finalSettings -> IgorHillshading finalSettings |> Ok
    | Error errorMessage -> Result.Error errorMessage
