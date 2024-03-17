module Demeton.Shaders.Pipeline.Common

open Demeton.DemTypes
open Raster
open Demeton.Shaders
open Demeton.Shaders.Types
open Png

type ShadingFuncId = string
type CompositingFuncId = string

type ShadingFuncFactory = ShadingFuncId -> RasterShader

type CompositingFuncFactory =
    CompositingFuncId -> AlphaCompositing.CompositingFunc

/// <summary>
/// Defines a single step in the shading pipeline.
/// </summary>
type ShadingStep =
    | ElevationColoring of ElevationColoring.Parameters
    | IgorHillshading of IgorHillshader.ShaderParameters
    | SlopeShading of SlopeShader.ShaderParameters
    | AspectShading of AspectShader.ShaderParameters
    | CustomShading of ShadingFuncId
    | Compositing of (ShadingStep * ShadingStep * CompositingFuncId)

[<Literal>]
let CompositingFuncIdOver = "over"

let createShadingFuncById shadingFuncId =
    invalidOp "we currently do not support custom shading functions"

let createCompositingFuncById compositingFuncId =
    match compositingFuncId with
    | CompositingFuncIdOver -> AlphaCompositing.imageOver
    | _ -> invalidOp "Unknown compositing function."


/// <summary>
/// Executes a shading step in the pipeline.
/// </summary>
/// <param name="shadingFuncFactory">
/// A function that creates a shading function given a shading function ID.
/// The function is used for CustomShading steps.
/// </param>
/// <param name="compositingFuncFactory">
/// A function that creates a compositing function given a compositing function
/// ID. The function is used for Compositing steps.
/// </param>
/// <param name="heightsArrays">A list of arrays of height values.</param>
/// <param name="srtmLevel">
/// The SRTM level to use for shading.
/// </param>
/// <param name="tileRect">The rectangle representing the tile.</param>
/// <param name="forward">A forward map projection function to use.</param>
/// <param name="inverse">An inverse map projection function to use.</param>
/// <param name="step">The shading step to execute.</param>
/// <returns>
/// A RawImageData object representing the result of the shading step.
/// </returns>
let rec executeShadingStep
    shadingFuncFactory
    compositingFuncFactory
    (heightsArrays: HeightsArray[])
    srtmLevel
    tileRect
    forward
    inverse
    (step: ShadingStep)
    : RawImageData =

    match step with
    | Compositing(step1, step2, compositingFuncId) ->
        let destImage =
            executeShadingStep
                shadingFuncFactory
                compositingFuncFactory
                heightsArrays
                srtmLevel
                tileRect
                forward
                inverse
                step1

        let sourceImage =
            executeShadingStep
                shadingFuncFactory
                compositingFuncFactory
                heightsArrays
                srtmLevel
                tileRect
                forward
                inverse
                step2

        Log.info $"Running compositing step '%s{compositingFuncId}'..."
        let compositingFunc = compositingFuncFactory compositingFuncId
        compositingFunc tileRect.Width tileRect.Height sourceImage destImage
    | _ ->
        let rasterShaderToUse =
            match step with
            | AspectShading parameters ->
                Log.info "Running aspect shading step..."

                Hillshading.shadeRaster
                    parameters.HeightsArrayIndex
                    (AspectShader.shadePixel parameters)
            | ElevationColoring parameters ->
                Log.info "Running elevation coloring step..."

                ElevationColoring.shadeRaster
                    parameters.HeightsArraysIndex
                    parameters.ColorScale
            | IgorHillshading parameters ->
                Log.info "Running igor hillshading step..."

                Hillshading.shadeRaster
                    parameters.HeightsArrayIndex
                    (IgorHillshader.shadePixel parameters)
            | SlopeShading parameters ->
                Log.info "Running slope shading step..."

                Hillshading.shadeRaster
                    parameters.HeightsArrayIndex
                    (SlopeShader.shadePixel parameters)
            | CustomShading shadingFuncId -> shadingFuncFactory shadingFuncId
            | _ -> invalidOp "Unsupported shading step"

        let imageData =
            Rgba8Bit.createImageData
                tileRect.Width
                tileRect.Height
                Rgba8Bit.ImageDataZero

        rasterShaderToUse
            heightsArrays
            srtmLevel
            tileRect
            imageData
            forward
            inverse

        imageData
