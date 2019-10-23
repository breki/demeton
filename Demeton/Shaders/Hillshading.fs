[<RequireQualifiedAccess>]
module Demeton.Shaders.Hillshading

open Png

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
