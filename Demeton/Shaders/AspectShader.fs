[<RequireQualifiedAccess>]
module Demeton.Shaders.AspectShader

open Demeton.Geometry.Common
open Demeton.Shaders
open Png

open System

type ShaderParameters =
    { NorthColor: Rgba8Bit.RgbaColor
      EastColor: Rgba8Bit.RgbaColor
      SouthColor: Rgba8Bit.RgbaColor
      WestColor: Rgba8Bit.RgbaColor
      HeightsArrayIndex: int }

let defaultParameters =
    { NorthColor = Rgba8Bit.rgbColor 255uy 0uy 0uy
      EastColor = Rgba8Bit.rgbColor 0uy 255uy 0uy
      SouthColor = Rgba8Bit.rgbColor 0uy 0uy 255uy
      WestColor = Rgba8Bit.rgbColor 0uy 255uy 255uy
      HeightsArrayIndex = 0 }

let shadePixel parameters : Hillshading.PixelHillshader =
    fun _ _ aspect ->
        let inline mixColors colorA colorB degrees minDegrees =
            Rgba8Bit.mixColors colorA colorB ((degrees - minDegrees) / 90.)

        match Double.IsNaN(aspect) with
        | true -> Rgba8Bit.TransparentColor
        | false ->
            let northColor = parameters.NorthColor
            let eastColor = parameters.EastColor
            let southColor = parameters.SouthColor
            let westColor = parameters.WestColor

            let aspectDeg = normalizeAngle (radToDeg aspect) 360.

            match aspectDeg with
            | x when x <= 90. -> mixColors northColor eastColor x 0.
            | x when x <= 180. -> mixColors eastColor southColor x 90.
            | x when x <= 270. -> mixColors southColor westColor x 180.
            | x -> mixColors westColor northColor x 270.
