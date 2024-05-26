[<RequireQualifiedAccess>]
module Demeton.Shaders.IgorHillshader

open Demeton.Shaders
open Demeton.Geometry.Common
open Png

open System

type ShaderParameters =
    {
      // https://en.wikipedia.org/wiki/Azimuth
      SunAzimuth: float
      ShadingColor: Rgba8Bit.RgbaColor
      Intensity: float
      HeightsArrayIndex: int }

[<Literal>]
let DefaultSunAzimuth = -45.

let defaultParameters =
    { SunAzimuth = degToRad DefaultSunAzimuth
      ShadingColor = 0u
      Intensity = 1.
      HeightsArrayIndex = 0 }

let shadePixel parameters : Hillshading.PixelHillshader =
    fun _ slope aspect ->
        match Double.IsNaN(aspect) with
        | true -> Rgba8Bit.TransparentColor
        | false ->
            let aspectDiff =
                differenceBetweenAngles
                    aspect
                    parameters.SunAzimuth
                    (Math.PI * 2.)

            let slopeDarkness = slope / (Math.PI / 2.)
            let aspectDarkness = aspectDiff / Math.PI
            let darkness = slopeDarkness * aspectDarkness * parameters.Intensity

            let alpha = Hillshading.colorComponentRatioToByte darkness

            parameters.ShadingColor |> Rgba8Bit.withAlpha alpha
