module Tests.Shaders.``Shading pipeline``

open Demeton.Shaders.Pipeline.Common
open Raster
open Demeton.Shaders.Types
open Png

open Xunit
open Swensen.Unquote
open Demeton.DemTypes

let mutable shadedImageGenerated = None

[<Literal>]
let ShadingFuncIdStupid = "stupid"

let stupidRasterShader: RasterShader = 
    fun heights rect imageData options -> 
    shadedImageGenerated <- Some imageData

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
            width height 
            (Rgba8Bit.ImageDataInitializer1D imageInitializer)
    compositedImageGenerated <- Some compositedImage
    compositedImage

let createCompositingFuncById compositingFuncId =
    match compositingFuncId with
    | CompositingFuncIdStupid -> stupidCompositing
    | _ -> invalidOp "Unknown compositing function."

[<Fact>]
let ``Supports running a simple, single-step pipeline``() =           
    let heights = 
        HeightsArray
            (10, 20, 40, 50, HeightsArrayInitializer1D(fun x -> DemHeightNone))
    let tileRect = { MinX = 10; MinY = 20; Width = 5; Height = 6 }

    let shaderOptions: ShaderOptions = { MapScale = 100000.; Dpi = 300. }

    let step = CustomShading ShadingFuncIdStupid

    let resultingImageData = 
        executeShadingStep 
            createShadingFuncById
            createCompositingFuncById 
            heights tileRect shaderOptions step
    test <@ Some resultingImageData = shadedImageGenerated @>

[<Fact>]
let ``Supports compositing of images``() =
    let stupidRasterShader: RasterShader = 
        fun _ _ _ _ -> ignore()       
            
    let heights = 
        HeightsArray
            (10, 20, 40, 50, HeightsArrayInitializer1D(fun x -> DemHeightNone))
    let tileRect = { MinX = 10; MinY = 20; Width = 5; Height = 6 }

    let shaderOptions: ShaderOptions = { MapScale = 100000.; Dpi = 300. }

    let shader1Step = CustomShading ShadingFuncIdStupid
    let shader2Step = CustomShading ShadingFuncIdStupid
    let compositingStep = 
        Compositing (shader1Step, shader2Step, CompositingFuncIdStupid)

    let resultingImageData = 
        executeShadingStep 
            createShadingFuncById
            createCompositingFuncById 
            heights 
            tileRect 
            shaderOptions 
            compositingStep
    test <@ Some resultingImageData = compositedImageGenerated @>
