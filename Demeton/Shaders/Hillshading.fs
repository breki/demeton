module Demeton.Shaders.Hillshading

open Demeton.Geometry.Common
open Demeton.Srtm.Funcs
open Png

open System;
open System.IO

type ShaderParameters = 
    { 
        SunAzimuth: float
        ShadingIntensity: float 
        ShadingColorR: byte
        ShadingColorG: byte
        ShadingColorB: byte
    }

type PixelHillshader = 
    ShaderParameters -> float -> float -> float -> Rgba8Bit.RgbaColor

let colorComponentRatioToByte (value: float): byte =
    (byte)(max (min ((int)(value * 255.)) 255) 0)


let igorHillshade: PixelHillshader = fun parameters  _  slope aspect ->
    match Double.IsNaN(aspect) with
    | true -> Rgba8Bit.TransparentColor
    | false ->
        let sunDirection = Math.PI * 3./2.

        let aspectDiff = differenceBetweenAngles
                            aspect 
                            (sunDirection - parameters.SunAzimuth)
                            (Math.PI * 2.)
    
        let slopeStrength = slope / (Math.PI / 2.)
        let aspectStrength = 1. - aspectDiff / Math.PI
        let shadowness = slopeStrength * aspectStrength

        let alpha = colorComponentRatioToByte 
                        (shadowness * parameters.ShadingIntensity)
    
        Rgba8Bit.rgbaColor
            parameters.ShadingColorR
            parameters.ShadingColorG
            parameters.ShadingColorB
            alpha

// todo: implement hillshade
let hillshade (bounds: LonLatBounds): Stream option =
    let neededTiles = boundsToTiles bounds

    None
