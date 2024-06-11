[<RequireQualifiedAccess>]
module Demeton.Shaders.SlopeShader

open Demeton.Geometry.Common
open Demeton.Shaders
open Demeton.Shaders.Types
open Png

open System

type ShaderParameters =
    { HorizontalColor: Rgba8Bit.RgbaColor
      VerticalColor: Rgba8Bit.RgbaColor
      Intensity: float
      DataSourceKey: string }

let defaultParameters =
    { HorizontalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy
      VerticalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 255uy
      Intensity = 1.
      DataSourceKey = DefaultDataSourceKey }

let shadePixel parameters : Hillshading.PixelHillshader =
    fun _ slope _ ->
        match Double.IsNaN(slope) with
        | true -> Rgba8Bit.TransparentColor
        | false ->
            let degrees = radToDeg slope

            let slopeIntensity = degrees / 90. * parameters.Intensity

            Rgba8Bit.mixColors
                parameters.HorizontalColor
                parameters.VerticalColor
                slopeIntensity
