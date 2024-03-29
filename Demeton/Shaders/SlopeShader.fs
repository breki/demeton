﻿[<RequireQualifiedAccess>]
module Demeton.Shaders.SlopeShader

open Demeton.Geometry.Common
open Demeton.Shaders
open Png

open System

type ShaderParameters =
    { HorizontalColor: Rgba8Bit.RgbaColor
      VerticalColor: Rgba8Bit.RgbaColor
      HeightsArrayIndex: int

    }

let defaultParameters =
    { HorizontalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy
      VerticalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 255uy
      HeightsArrayIndex = 0 }

let shadePixel parameters : Hillshading.PixelHillshader =
    fun _ slope _ ->
        match Double.IsNaN(slope) with
        | true -> Rgba8Bit.TransparentColor
        | false ->
            let degrees = radToDeg slope

            Rgba8Bit.mixColors
                parameters.HorizontalColor
                parameters.VerticalColor
                (degrees / 90.)
