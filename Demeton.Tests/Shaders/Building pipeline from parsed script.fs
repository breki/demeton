module Tests.Shaders.``Building pipeline from parsed script``

open Demeton.Shaders
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Png

open Xunit
open Swensen.Unquote
open TestHelp
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


let private testRegisteredStepBuilders = dict [
    ("shader1", fun _ -> CustomShading "shaderfunc1" |> Ok)
    ("shader2", fun _ -> CustomShading "shaderfunc2" |> Ok)
    ("shader3", fun _ -> CustomShading "shaderfunc3" |> Ok)
]

let buildShadingPipeline 
    (registeredStepBuilders: IDictionary<string, ShadingStepBuildingFunc>) 
    (parsedScript: ParsedScript) =
    let pipeline = 
        parsedScript
        |> List.fold (fun pipeline (parsedStep: ParsedStep) -> 
            let stepBuilder = registeredStepBuilders.[parsedStep.Name]
            let shaderStepMaybe = stepBuilder parsedStep

            match shaderStepMaybe with
            | Ok shaderStep ->
                match pipeline with
                | None -> Some shaderStep 
                | Some pipelineStep ->
                    Compositing (pipelineStep, shaderStep, CompositingFuncIdOver)
                    |> Some
            | Error stepBuildingErrorMessage -> invalidOp "todo"
            ) None

    match pipeline with
    | None -> Error "Shading pipeline is empty."
    | Some rootStep -> Ok rootStep

let private rootStep (result: Result<ShadingStep, string>) =
    match result with
    | Ok rootStep -> rootStep
    | _ -> fail "The result indicates an error."

let private isCustomShader shadingFuncId step =
    match step with
    | CustomShading funcId -> shadingFuncId = funcId
    | _ -> false

let private isCompositing step =
    match step with
    | Compositing _ -> true
    | _ -> false

let private compositing step =
    match step with
    | Compositing (step1, step2, compositingFunc) -> 
        (step1, step2, compositingFunc)
    | _ -> fail "The step is not Compositing step."

[<Fact>]
let ``Reports an error is pipeline is empty``() =
    let result: Result<ShadingStep, string> = 
        buildShadingPipeline testRegisteredStepBuilders []

    test <@ result |> isErrorData "Shading pipeline is empty." @>

[<Fact>]
let ``Supports a single-step pipeline without any arguments``() =
    let parsedScript = [ { Name = "shader1"; Parameters = [] } ]

    let result: Result<ShadingStep, string> = 
        buildShadingPipeline testRegisteredStepBuilders parsedScript

    test <@ result |> rootStep |> isCustomShader "shaderfunc1" @>

[<Fact>]
let ``Two steps are combined using Compositing step``() =
    let parsedScript = [ 
        { Name = "shader1"; Parameters = [] } 
        { Name = "shader2"; Parameters = [] } 
        ]
    
    let result: Result<ShadingStep, string> = 
        buildShadingPipeline testRegisteredStepBuilders parsedScript

    test <@ result |> rootStep |> isCompositing @>

    // todo: how to know which step is which? we can't compare funcs
    let (step1, step2, compositingFuncId) = 
        result |> rootStep |> compositing

    test <@ step1 |> isCustomShader "shaderfunc1" @>
    test <@ step2 |> isCustomShader "shaderfunc2" @>
    test <@ compositingFuncId = CompositingFuncIdOver @>

[<Fact>]
let ``Three steps are combined using Compositing step``() =
    let parsedScript = [ 
        { Name = "shader1"; Parameters = [] } 
        { Name = "shader2"; Parameters = [] } 
        { Name = "shader3"; Parameters = [] } 
        ]
    
    let result: Result<ShadingStep, string> = 
        buildShadingPipeline testRegisteredStepBuilders parsedScript

    test <@ result |> rootStep |> isCompositing @>

    let (compositingStep1, step3, _) = 
        result |> rootStep |> compositing

    test <@ compositingStep1 |> isCompositing @>
    test <@ step3 |> isCustomShader "shaderfunc3" @>

    let (step1, step2, _) = compositingStep1 |> compositing

    test <@ step1 |> isCustomShader "shaderfunc1" @>
    test <@ step2 |> isCustomShader "shaderfunc2" @>

[<Fact>]
let ``Can parse elevation coloring step without parameters``() =
    let parsedStep = { Name = "elecolor"; Parameters = [] } 

    let step = elevationColoringStepBuilder parsedStep
    test
        <@ step =
            Ok
                (ElevationColoring
                    ({ ColorScale = ElevationColoring.colorScaleMaperitive })) 
        @>

[<Fact>]
let ``Can parse elevation coloring step with valid parameters``() =
    let parsedStep = 
        { Name = "elecolor"; 
        Parameters = 
            [ { Name = "scale"; 
                Value = "-1:#000000;2000:#ffffff;none:#000000" } ] } 

    let expectedColorScale: ElevationColoring.ColorScale = 
        {
            Marks = [| 
                -1s, Rgba8Bit.rgbColor 0uy 0uy 0uy 
                2000s, Rgba8Bit.rgbColor 0xffuy 0xffuy 0xffuy 
                |]
            NoneColor = Rgba8Bit.rgbColor 0uy 0uy 0uy
        }

    let step = elevationColoringStepBuilder parsedStep
    test <@ step = Ok (ElevationColoring { ColorScale = expectedColorScale }) 
        @>

[<Fact>]
let ``Reports an error when color scale parameter is invalid``() =
    let parsedStep = 
        { Name = "elecolor"; 
        Parameters = 
            [ { Name = "scale"; Value = "-1:#000000;2000:#ffffff" } ] } 

    let step = elevationColoringStepBuilder parsedStep
    test <@ step = 
        Error 
            ("Error in step 'elecolor': 'scale' parameter value error: " 
                + "invalid color scale.") @>
    
[<Fact>]
let ``Reports an error when parameter is not recognized``() =
    let parsedStep = 
        { Name = "elecolor"; 
        Parameters = 
            [ { Name = "somepar"; Value = "some value" } ] } 

    let step = elevationColoringStepBuilder parsedStep
    test <@ step = 
        Error 
            "Error in step 'elecolor': 'somepar' parameter is not recognized." 
            @>
    
[<Fact>]
let ``Can handle parsing of remaining parameters when error is found``() =
    let parsedStep = 
        { Name = "elecolor"; 
        Parameters = 
            [ { Name = "somepar"; Value = "some value" };
            { Name = "somepar2"; Value = "some value" } ] } 

    let step = elevationColoringStepBuilder parsedStep
    test <@ step = 
        Error 
            "Error in step 'elecolor': 'somepar' parameter is not recognized." 
            @>

