module Demeton.Shaders.Pipeline.Common

open Raster
open Demeton.Shaders
open Demeton.Shaders.Types
open Png

type ShadingFuncId = string
type CompositingFuncId = string

type ShadingFuncFactory = ShadingFuncId -> RasterShader

type CompositingFuncFactory = 
    CompositingFuncId -> AlphaCompositing.CompositingFunc

type ShadingStep =
    | ElevationColoring of ElevationColoring.Parameters
    | IgorHillshading of IgorHillshader.ShaderParameters
    | SlopeShading of SlopeShader.ShaderParameters
    | AspectShading of AspectShader.ShaderParameters
    | CustomShading of ShadingFuncId
    | Compositing of (ShadingStep * ShadingStep * CompositingFuncId)

[<Literal>]
let CompositingFuncIdOver = "over"

let createShadingFuncById shadingFuncId 
    = invalidOp "we currently do not support custom shading functions"

let createCompositingFuncById compositingFuncId =
    match compositingFuncId with
    | CompositingFuncIdOver -> Png.AlphaCompositing.imageOver
    | _ -> invalidOp "Unknown compositing function."


let rec executeShadingStep
    shadingFuncFactory
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
                shadingFuncFactory
                compositingFuncFactory 
                heightsArray 
                tileRect 
                shaderOptions 
                step1
        let image2 = 
            executeShadingStep 
                shadingFuncFactory
                compositingFuncFactory 
                heightsArray 
                tileRect 
                shaderOptions 
                step2

        Log.info "Running compositing step '%s'..." compositingFuncId
        let compositingFunc = compositingFuncFactory compositingFuncId
        compositingFunc tileRect.Width tileRect.Height image1 image2
    | _ -> 
        let rasterShaderToUse = 
            match step with
            | AspectShading parameters ->
                Log.info "Running aspect shading step..."
                Hillshading.shadeRaster (AspectShader.shadePixel parameters)
            | ElevationColoring parameters -> 
                Log.info "Running elevation coloring step..."
                ElevationColoring.shadeRaster parameters.ColorScale
            | IgorHillshading parameters -> 
                Log.info "Running igor hillshading step..."
                Hillshading.shadeRaster (IgorHillshader.shadePixel parameters)
            | SlopeShading parameters ->
                Log.info "Running slope shading step..."
                Hillshading.shadeRaster (SlopeShader.shadePixel parameters)
            | CustomShading shadingFuncId -> shadingFuncFactory shadingFuncId
            | _ -> invalidOp "Unsupported shading step"

        let imageData =
            Rgba8Bit.createImageData 
                tileRect.Width tileRect.Height Rgba8Bit.ImageDataZero

        rasterShaderToUse heightsArray tileRect imageData shaderOptions
        imageData
