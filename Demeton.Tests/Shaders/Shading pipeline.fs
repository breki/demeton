module Tests.Shaders.``Shading pipeline``

open Demeton.Shaders.ShadingPipeline
open Raster
open Demeton.Shaders.Types
open Png

open Xunit
open Swensen.Unquote
open Demeton.DemTypes


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
