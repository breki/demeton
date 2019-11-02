module Demeton.Shaders.Pipeline.BuildingElevationColoring

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Building

let elevationColoringStepBuilder: ShadingStepBuildingFunc = fun parsedStep ->
    let parseScale scaleString settings =
        let parsingResult = ElevationColoring.tryParseScale scaleString
        match parsingResult with 
        | Ok colorScale -> 
            Ok { settings with ColorScale = colorScale }
        | Error errorMessage -> Error errorMessage

    let parDefs = dict [
        "scale", parseScale
        ]

    let defaultParameters = 
        { ColorScale = ElevationColoring.colorScaleMaperitive }

    let stepBuildingResult = stepBuilder parsedStep parDefs defaultParameters
    match stepBuildingResult with
    | Ok finalSettings -> ElevationColoring finalSettings |> Ok
    | Error errorMessage -> Error errorMessage
