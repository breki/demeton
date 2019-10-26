module Tests.Shaders.``Shading pipeline``

open Raster
open Demeton.Shaders
open Demeton.Shaders.Types
open Demeton.Geometry.Common

open Xunit
open Swensen.Unquote
open Png
open Demeton.DemTypes

type CompositingFunc = 
    int -> int -> RawImageData -> RawImageData -> RawImageData

type ShadingStep =
    | Shading of RasterShader
    | Compositing of (ShadingStep * ShadingStep * CompositingFunc)

let alphaCompositingOver: CompositingFunc = 
    fun
        (imageWidth: int)
        (imageHeight: int)
        (source: RawImageData)
        (dest: RawImageData) ->
    for y in 0 .. imageHeight - 1 do
        for x in 0 .. imageWidth - 1 do
            let sourcePixel = Rgba8Bit.pixelAt source imageWidth x y
            let destPixel = Rgba8Bit.pixelAt dest imageWidth x y

            let outPixel = AlphaCompositing.over sourcePixel destPixel
            Rgba8Bit.setPixelAt dest imageWidth x y outPixel

    dest

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
