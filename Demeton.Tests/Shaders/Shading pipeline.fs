module Tests.Shaders.``Shading pipeline``

open Raster
open Demeton.Shaders.Types
open Png

open Xunit
open Swensen.Unquote
open Png
open Demeton.DemTypes


type ShadingStep =
    | Shading of RasterShader
    | Compositing of 
        (ShadingStep * ShadingStep * AlphaCompositing.CompositingFunc)


let rec executeShadingStep 
    heightsArray
    tileRect
    shaderOptions
    (step: ShadingStep)
    : RawImageData =

    match step with
    | Shading rasterShader -> 
        let imageData =
            Rgba8Bit.createImageData 
                tileRect.Width tileRect.Height Rgba8Bit.ImageDataZero

        rasterShader heightsArray tileRect imageData shaderOptions
        imageData

    | Compositing (step1, step2, compositingFunc) ->
        let image1 = 
            executeShadingStep heightsArray tileRect shaderOptions step1
        let image2 = 
            executeShadingStep heightsArray tileRect shaderOptions step2

        compositingFunc tileRect.Width tileRect.Height image1 image2


[<Fact>]
let ``Supports running a simple, single-step pipeline``() =
    let mutable shadedImageGenerated = None

    let stupidRasterShader: RasterShader = 
        fun heights rect imageData options -> 
        shadedImageGenerated <- Some imageData
            
    let heights = 
        HeightsArray
            (10, 20, 40, 50, HeightsArrayInitializer1D(fun x -> DemHeightNone))
    let tileRect = { MinX = 10; MinY = 20; Width = 5; Height = 6 }

    let shaderOptions: ShaderOptions = { MapScale = 100000.; Dpi = 300. }

    let step = Shading(stupidRasterShader)

    let resultingImageData = 
        executeShadingStep heights tileRect shaderOptions step
    test <@ Some resultingImageData = shadedImageGenerated @>

[<Fact>]
let ``Supports compositing of images``() =
    let mutable compositedImageGenerated = None

    let stupidRasterShader: RasterShader = 
        fun _ _ _ _ -> ignore()

    let stupidCompositing (width: int) (height: int) _ _ =
        let imageInitializer _ = Rgba8Bit.rgbaColor 1uy 1uy 1uy 1uy

        let compositedImage =
            Rgba8Bit.createImageData 
                width height 
                (Rgba8Bit.ImageDataInitializer1D imageInitializer)
        compositedImageGenerated <- Some compositedImage
        compositedImage
        
            
    let heights = 
        HeightsArray
            (10, 20, 40, 50, HeightsArrayInitializer1D(fun x -> DemHeightNone))
    let tileRect = { MinX = 10; MinY = 20; Width = 5; Height = 6 }

    let shaderOptions: ShaderOptions = { MapScale = 100000.; Dpi = 300. }

    let shader1Step = Shading(stupidRasterShader)
    let shader2Step = Shading(stupidRasterShader)
    let compositingStep = 
        Compositing (shader1Step, shader2Step, stupidCompositing)

    let resultingImageData = 
        executeShadingStep heights tileRect shaderOptions compositingStep
    test <@ Some resultingImageData = compositedImageGenerated @>
