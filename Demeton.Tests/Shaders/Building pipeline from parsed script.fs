module Tests.Shaders.``Building pipeline from parsed script``

open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing

open Xunit
open Swensen.Unquote
open TestHelp
open System.Collections.Generic

type ShadingStepBuildingFunc = ParsedStep -> ShadingStep

let elevationColoringStepBuilder: ShadingStepBuildingFunc = fun parsedStep ->
    let parametersBuilder (parsedParameters: ParsedParameter list)
        : ElevationColoringParameters =
        let mutable pars = 
            { ColorScale = 
                Demeton.Shaders.ElevationColoring.colorScaleMaperitive }

        for parsedParameter in parsedParameters do
            match parsedParameter.Name with
            | "scale" -> invalidOp "todo"
            | _ -> invalidOp "todo"

        pars

    ElevationColoring (parametersBuilder parsedStep.Parameters)


let private testRegisteredStepBuilders = dict [
    ("shader1", fun _ -> CustomShading "shaderfunc1")
    ("shader2", fun _ -> CustomShading "shaderfunc2")
    ("shader3", fun _ -> CustomShading "shaderfunc3")
]

let buildShadingPipeline 
    (registeredStepBuilders: IDictionary<string, ShadingStepBuildingFunc>) 
    (parsedScript: ParsedScript) =
    let pipeline = 
        parsedScript
        |> List.fold (fun pipeline (parsedStep: ParsedStep) -> 
            let stepBuilder = registeredStepBuilders.[parsedStep.Name]
            let shaderStep = stepBuilder parsedStep

            match pipeline with
            | None -> Some shaderStep 
            | Some pipelineStep ->
                Compositing (pipelineStep, shaderStep, CompositingFuncIdOver)
                |> Some
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
let ``Can successfully parse elevation coloring step without parameters``() =
    let parsedStep = { Name = "elecolor"; Parameters = [] } 

    let step = elevationColoringStepBuilder parsedStep
    test <@ step = ElevationColoring { 
                ColorScale = 
                    Demeton.Shaders.ElevationColoring.colorScaleMaperitive } 
        @>
