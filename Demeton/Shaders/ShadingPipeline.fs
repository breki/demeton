module Demeton.Shaders.ShadingPipeline

open Raster
open Demeton.Shaders.Types
open Png


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
