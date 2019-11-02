module Demeton.Shaders.Pipeline.Building

open Common
open Parsing
open System.Collections.Generic

type ShadingStepBuildingFunc = ParsedStep -> Result<ShadingStep, string>

type StepParameterParsingResult<'T> = Result<'T, string>
type StepParameterParser<'T> = string -> 'T -> StepParameterParsingResult<'T>

let stepBuilder 
    parsedStep
    (parametersDefinitions: IDictionary<string, StepParameterParser<'T>>)
    (defaultParAccumulator: 'T)
    : Result<'T, string> 
    =
    let tryFindParameterDefinition parameterName =
        let found, parameterDefinition = 
            parametersDefinitions.TryGetValue(parameterName)
        match found with
        | false -> None
        | true -> Some parameterDefinition

    let collectParameters 
        (state: Result<'T, string>) 
        (parsedParameter: ParsedParameter) 
        = 
        match state with
        | Ok parAccumulator ->
            let parameterName = parsedParameter.Name

            match tryFindParameterDefinition parameterName with
            | Some parameterParser ->
                let parsingResult = 
                    parameterParser parsedParameter.Value parAccumulator 
                match parsingResult with 
                | Ok newParAccumulator -> Ok newParAccumulator
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
        |> List.fold collectParameters (Ok defaultParAccumulator)

    match parametersCollectingResult with
    | Ok parAccumulator -> Ok parAccumulator
    | Error parametersParsingErrorMessage -> 
        let completeMessage =
            sprintf 
                "Error in step '%s': %s." 
                parsedStep.Name 
                parametersParsingErrorMessage
        Error completeMessage
