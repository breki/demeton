﻿[<RequireQualifiedAccess>]
module Demeton.Shaders.IgorHillshader

open Demeton.Shaders
open Demeton.Geometry.Common
open Png

open System

type ShaderParameters = 
    { 
        SunAzimuth: float
        ShadingColorR: byte
        ShadingColorG: byte
        ShadingColorB: byte
    }

let shadePixel parameters: Hillshading.PixelHillshader = fun _  slope aspect ->
    match Double.IsNaN(aspect) with
    | true -> Rgba8Bit.TransparentColor
    | false ->
        let sunDirection = degToRad 180.

        let aspectDiff = differenceBetweenAngles
                            aspect 
                            (sunDirection - parameters.SunAzimuth)
                            (Math.PI * 2.)
    
        let slopeStrength = slope / (Math.PI / 2.)
        let aspectStrength = 1. - aspectDiff / Math.PI
        let shadowness = slopeStrength * aspectStrength

        let alpha = Hillshading.colorComponentRatioToByte shadowness
    
        Rgba8Bit.rgbaColor
            parameters.ShadingColorR
            parameters.ShadingColorG
            parameters.ShadingColorB
            alpha