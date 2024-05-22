[<RequireQualifiedAccess>]
module Demeton.Shaders.HighwireHillshader

open Demeton.Geometry.Common
open System
open Png

type ShaderParameters =
    {
      // https://en.wikipedia.org/wiki/Azimuth
      SunAzimuth: float
      HeightsArrayIndex: int }

[<Literal>]
let DefaultSunAzimuth = -45.

let defaultParameters =
    { SunAzimuth = degToRad DefaultSunAzimuth
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
            let darkness = slopeDarkness * aspectDarkness

            if aspectDarkness > 1. || aspectDarkness < 0. then
                Rgba8Bit.rgbColor 255uy 0uy 0uy
            else
                let darknessByte =
                    255uy - Hillshading.colorComponentRatioToByte darkness

                Rgba8Bit.rgbColor darknessByte darknessByte darknessByte
