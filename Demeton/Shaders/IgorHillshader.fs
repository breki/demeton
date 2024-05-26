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
      SunAltitude: float
      ShadingColor: Rgba8Bit.RgbaColor
      Intensity: float
      HeightsArrayIndex: int }

[<Literal>]
let DefaultSunAzimuth = -45.

[<Literal>]
let DefaultSunAltitude = 45.

let defaultParameters =
    { SunAzimuth = DefaultSunAzimuth |> degToRad
      SunAltitude = DefaultSunAltitude |> degToRad
      ShadingColor = 0u
      Intensity = 1.
      HeightsArrayIndex = 0 }

let shadePixel parameters : Hillshading.PixelHillshader =
    fun _ slope aspect ->
        match Double.IsNaN(aspect) with
        | true -> Rgba8Bit.TransparentColor
        | false ->
            // let aspectDiff =
            //     differenceBetweenAngles
            //         aspect
            //         parameters.SunAzimuth
            //         (Math.PI * 2.)
            //
            // // "darkness" is a value from 0 (bright) to 1 (dark)
            // let slopeDarkness = slope / (Math.PI / 2.)
            // let aspectDarkness = aspectDiff / Math.PI
            // let darkness = slopeDarkness * aspectDarkness * parameters.Intensity

            let cosSolarElevation = sin parameters.SunAltitude
            let sinSolarElevation = cos parameters.SunAltitude
            let cosAspectMinusAzimuth = cos (aspect - parameters.SunAzimuth)
            let sinSlope = sin slope

            let darkness =
                (cosSolarElevation * cos slope)
                + (sinSolarElevation * sinSlope * cosAspectMinusAzimuth)
                |> max 0.0
                |> min 1.0 // Ensure positive values

            let darkness = 1. - darkness

            let alpha = Hillshading.colorComponentRatioToByte darkness

            parameters.ShadingColor |> Rgba8Bit.withAlpha alpha
