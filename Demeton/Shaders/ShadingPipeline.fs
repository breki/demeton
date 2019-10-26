module Demeton.Shaders.ShadingPipeline

open Raster
open Demeton.Shaders.Types
open Png

type ShadingStep =
    | ElevationColoring
    | IgorHillshading of IgorHillshader.ShaderParameters
    | CustomShading of RasterShader
    | Compositing of 
        (ShadingStep * ShadingStep * AlphaCompositing.CompositingFunc)


let rec executeShadingStep 
    heightsArray
    tileRect
    shaderOptions
    (step: ShadingStep)
    : RawImageData =

    match step with
    | Compositing (step1, step2, compositingFunc) ->
        let image1 = 
            executeShadingStep heightsArray tileRect shaderOptions step1
        let image2 = 
            executeShadingStep heightsArray tileRect shaderOptions step2

        compositingFunc tileRect.Width tileRect.Height image1 image2
    | _ -> 
        let rasterShaderToUse = 
            match step with
            | ElevationColoring -> ElevationColoring.shadeRaster
            | IgorHillshading parameters -> 
                Hillshading.shadeRaster (IgorHillshader.shadePixel parameters)
            | CustomShading rasterShader -> rasterShader
            | _ -> invalidOp "Unsupported shading step"

        let imageData =
            Rgba8Bit.createImageData 
                tileRect.Width tileRect.Height Rgba8Bit.ImageDataZero

        rasterShaderToUse heightsArray tileRect imageData shaderOptions
        imageData
