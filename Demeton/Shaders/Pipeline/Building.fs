module Demeton.Shaders.Pipeline.Building

open Common
open Parsing
open System.Collections.Generic

type ShadingStepBuildingFunc = ParsedStep -> Result<ShadingStep, string>

type StepParameterParsingResult<'TSettings> = Result<'TSettings, string>

type StepParameterParser<'TSettings> = 
    string -> 'TSettings -> StepParameterParsingResult<'TSettings>

let stepBuilder 
    parsedStep
    (parametersDefinitions: 
        IDictionary<string, StepParameterParser<'TSettings>>)
    (defaultSettings: 'TSettings)
    : Result<'TSettings, string> 
    =
    let tryFindParameterDefinition parameterName =
        let found, parameterDefinition = 
            parametersDefinitions.TryGetValue(parameterName)
        match found with
        | false -> None
        | true -> Some parameterDefinition

    let collectParameters 
        (collectingState: Result<'TSettings, string>) 
        (parsedParameter: ParsedParameter) 
        = 
        match collectingState with
        | Ok settingsSoFar ->
            let parameterName = parsedParameter.Name

            match tryFindParameterDefinition parameterName with
            | Some parameterParser ->
                let parsingResult = 
                    parameterParser parsedParameter.Value settingsSoFar 
                match parsingResult with 
                | Ok newSettings -> Ok newSettings
                | Error errorMessage -> 
                    let parameterErrorMessage = 
                        sprintf 
                            "'%s' parameter value error: %s" 
                            parameterName
                            errorMessage
                    Error parameterErrorMessage
            | None -> 
                let errorMessage = 
                    sprintf 
                        "'%s' parameter is not recognized" 
                        parameterName
                Error errorMessage
        | Error errorMessage -> Error errorMessage

    let parametersCollectingResult = 
        parsedStep.Parameters 
        |> List.fold collectParameters (Ok defaultSettings)

    match parametersCollectingResult with
    | Ok settings -> Ok settings
    | Error parametersParsingErrorMessage -> 
        let completeMessage =
            sprintf 
                "Error in step '%s': %s." 
                parsedStep.Name 
                parametersParsingErrorMessage
        Error completeMessage


let buildShadingPipeline 
    (registeredStepBuilders: IDictionary<string, ShadingStepBuildingFunc>) 
    (parsedScript: ParsedScript) =

    let foldPipeline state parsedStep =
        match state with
        | Error errorMessage -> Error errorMessage
        | Ok previousStep ->
            let stepName = parsedStep.Name
            let stepBuilderMaybe = registeredStepBuilders.TryGetValue stepName

            match stepBuilderMaybe with
            | true, stepBuilder -> 
                let shaderStepMaybe = stepBuilder parsedStep

                match shaderStepMaybe with
                | Ok shaderStep ->
                    match previousStep with
                    | None -> Some shaderStep |> Ok 
                    | Some pipelineStep ->
                        Compositing 
                            (pipelineStep, shaderStep, CompositingFuncIdOver)
                        |> Some |> Ok
                | Error stepBuildingErrorMessage -> 
                    Error stepBuildingErrorMessage
            | false, _ ->
                Error (sprintf "Unrecognized shading step '%s'." stepName)


    let pipelineFoldingResult = 
        parsedScript |> List.fold foldPipeline (Ok None)

    match pipelineFoldingResult with
    | Error errorMessage -> Error errorMessage
    | Ok None -> Error "Shading pipeline is empty."
    | Ok (Some rootStep) -> Ok rootStep
