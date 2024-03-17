module Tests.Shaders.``Shading pipeline``

open Demeton.Shaders.Pipeline.Common
open Demeton.Shaders
open Demeton.Shaders.Types
open Png

open Xunit
open Swensen.Unquote

let mutable shadedImageGenerated = None

[<Literal>]
let ShadingFuncIdStupid = "stupid"

let stupidRasterShader: RasterShader =
    fun _ _ _ imageData _ _ -> shadedImageGenerated <- Some imageData

let createShadingFuncById shadingFuncId =
    match shadingFuncId with
    | ShadingFuncIdStupid -> stupidRasterShader
    | _ -> invalidOp "Unknown shading function."

[<Literal>]
let CompositingFuncIdStupid = "stupid"

let mutable compositedImageGenerated = None

let stupidCompositing (width: int) (height: int) _ _ =
    let imageInitializer _ = Rgba8Bit.rgbaColor 1uy 1uy 1uy 1uy

    let compositedImage =
        Rgba8Bit.createImageData
            width
            height
            (Rgba8Bit.ImageDataInitializer1D imageInitializer)

    compositedImageGenerated <- Some compositedImage
    compositedImage

let createCompositingFuncById compositingFuncId =
    match compositingFuncId with
    | CompositingFuncIdStupid -> stupidCompositing
    | _ -> invalidOp "Unknown compositing function."

let area, heightsArray, srtmLevel, mapProjection, mapScale, tileRect =
    ShadingSampleGenerator.generateSample ()

[<Fact>]
let ``Supports running a simple, single-step pipeline`` () =
    let step = CustomShading ShadingFuncIdStupid

    let resultingImageData =
        executeShadingStep
            createShadingFuncById
            createCompositingFuncById
            [| heightsArray |]
            srtmLevel
            tileRect
            mapProjection.Proj
            mapProjection.Invert
            step

    test <@ Some resultingImageData = shadedImageGenerated @>

[<Fact>]
let ``Supports compositing of images`` () =
    let shader1Step = CustomShading ShadingFuncIdStupid
    let shader2Step = CustomShading ShadingFuncIdStupid

    let compositingStep =
        Compositing(shader1Step, shader2Step, CompositingFuncIdStupid)

    let resultingImageData =
        executeShadingStep
            createShadingFuncById
            createCompositingFuncById
            [| heightsArray |]
            srtmLevel
            tileRect
            mapProjection.Proj
            mapProjection.Invert
            compositingStep

    test <@ Some resultingImageData = compositedImageGenerated @>

[<Fact>]
let ``Supports elevation coloring`` () =
    let step = ElevationColoring ElevationColoring.defaultParameters

    executeShadingStep
        createShadingFuncById
        createCompositingFuncById
        [| heightsArray |]
        srtmLevel
        tileRect
        mapProjection.Proj
        mapProjection.Invert
        step
    |> ignore

[<Fact>]
let ``Supports aspect shading`` () =
    let step = AspectShading AspectShader.defaultParameters

    executeShadingStep
        createShadingFuncById
        createCompositingFuncById
        [| heightsArray |]
        srtmLevel
        tileRect
        mapProjection.Proj
        mapProjection.Invert
        step
    |> ignore

[<Fact>]
let ``Supports slope shading`` () =
    let step = SlopeShading SlopeShader.defaultParameters

    executeShadingStep
        createShadingFuncById
        createCompositingFuncById
        [| heightsArray |]
        srtmLevel
        tileRect
        mapProjection.Proj
        mapProjection.Invert
        step
    |> ignore

[<Fact>]
let ``Supports igor shading`` () =
    let step = IgorHillshading IgorHillshader.defaultParameters

    executeShadingStep
        createShadingFuncById
        createCompositingFuncById
        [| heightsArray |]
        srtmLevel
        tileRect
        mapProjection.Proj
        mapProjection.Invert
        step
    |> ignore
