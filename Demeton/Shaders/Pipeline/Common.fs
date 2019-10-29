module Demeton.Shaders.Pipeline.Common

open Raster
open Demeton.Shaders
open Demeton.Shaders.Types
open Png

type ElevationColoringParameters = { ColorScale: ElevationColoring.ColorScale }

type CompositingFuncId = string

type CompositingFuncFactory = 
    CompositingFuncId -> AlphaCompositing.CompositingFunc

type ShadingStep =
    | ElevationColoring of ElevationColoringParameters
    | IgorHillshading of IgorHillshader.ShaderParameters
    | CustomShading of RasterShader
    | Compositing of (ShadingStep * ShadingStep * CompositingFuncId)

[<Literal>]
let CompositingFuncIdOver = "over"

let createCompositingFuncById compositingFuncId =
    match compositingFuncId with
    | CompositingFuncIdOver -> Png.AlphaCompositing.imageOver
    | _ -> invalidOp "Unknown compositing function."


let rec executeShadingStep 
    compositingFuncFactory
    heightsArray
    tileRect
    shaderOptions
    (step: ShadingStep)
    : RawImageData =

    match step with
    | Compositing (step1, step2, compositingFuncId) ->
        let image1 = 
            executeShadingStep 
                compositingFuncFactory heightsArray tileRect shaderOptions step1
        let image2 = 
            executeShadingStep 
                compositingFuncFactory heightsArray tileRect shaderOptions step2

        let compositingFunc = compositingFuncFactory compositingFuncId
        compositingFunc tileRect.Width tileRect.Height image1 image2
    | _ -> 
        let rasterShaderToUse = 
            match step with
            | ElevationColoring parameters -> 
                ElevationColoring.shadeRaster parameters.ColorScale
            | IgorHillshading parameters -> 
                Hillshading.shadeRaster (IgorHillshader.shadePixel parameters)
            | CustomShading rasterShader -> rasterShader
            | _ -> invalidOp "Unsupported shading step"

        let imageData =
            Rgba8Bit.createImageData 
                tileRect.Width tileRect.Height Rgba8Bit.ImageDataZero

        rasterShaderToUse heightsArray tileRect imageData shaderOptions
        imageData
