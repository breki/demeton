module Tests.Shaders.``Building pipeline from parsed script``

open Demeton.Shaders.Types
open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders.Pipeline.Parsing
open Png

open Xunit
open Swensen.Unquote
open TestHelp

let stupidRasterShader: RasterShader =  fun _ _ _ _ -> ()

let buildShadingPipeline (parsedScript: ParsedScript) =
    let pipeline = 
        parsedScript
        |> List.fold (fun pipeline parsedStep -> 
            match pipeline with
            | None -> CustomShading stupidRasterShader |> Some
            | Some pipelineStep ->
                let currentStep = CustomShading stupidRasterShader
                Compositing 
                    (pipelineStep, currentStep, AlphaCompositing.imageOver)
                |> Some
            ) None

    match pipeline with
    | None -> Error "Shading pipeline is empty."
    | Some rootStep -> Ok rootStep

let rootStep (result: Result<ShadingStep, string>) =
    match result with
    | Ok rootStep -> rootStep
    | _ -> fail "The result indicates an error."

let isCustomShader step =
    match step with
    | CustomShading _ -> true
    | _ -> false

let isCompositing step =
    match step with
    | Compositing _ -> true
    | _ -> false

let compositing step =
    match step with
    | Compositing (step1, step2, compositingFunc) -> 
        (step1, step2, compositingFunc)
    | _ -> fail "The step is not Compositing step."

[<Fact>]
let ``Reports an error is pipeline is empty``() =
    let result: Result<ShadingStep, string> = buildShadingPipeline []

    test <@ result |> isErrorData "Shading pipeline is empty." @>

[<Fact>]
let ``Supports a single-step pipeline without any arguments``() =
    let parsedScript = [ { Name = "step1"; Parameters = [] } ]

    let result: Result<ShadingStep, string> = buildShadingPipeline parsedScript

    test <@ result |> rootStep |> isCustomShader @>

[<Fact>]
let ``Two steps are combined using Compositing step``() =
    let parsedScript = [ 
        { Name = "step1"; Parameters = [] } 
        { Name = "step2"; Parameters = [] } 
        ]
    
    let result: Result<ShadingStep, string> = buildShadingPipeline parsedScript

    test <@ result |> rootStep |> isCompositing @>
    test <@ 
            // todo: how to know which step is which? we can't compare funcs
            let (step1, step2, compositingFunc) = 
                result |> rootStep |> compositing
            step1 |> isCustomShader && step2 |> isCustomShader
        @>

[<Fact>]
let ``Three steps are combined using Compositing step``() =
    let parsedScript = [ 
        { Name = "step1"; Parameters = [] } 
        { Name = "step2"; Parameters = [] } 
        { Name = "step3"; Parameters = [] } 
        ]
    
    let result: Result<ShadingStep, string> = buildShadingPipeline parsedScript

    test <@ result |> rootStep |> isCompositing @>

    let (compositingStep1, step3, compositingFunc) = 
        result |> rootStep |> compositing

    test <@ compositingStep1 |> isCompositing && step3 |> isCustomShader @>

    let (step1, step2, compositingFunc) = compositingStep1 |> compositing

    test <@ step1 |> isCustomShader && step2 |> isCustomShader @>

