﻿[<RequireQualifiedAccess>]
module Demeton.Shaders.SlopeShader

open Demeton.Geometry.Common
open Demeton.Shaders.Hillshading
open Png

open System

let run: PixelHillshaderFunc = fun _  _  slope _ ->
    match Double.IsNaN(slope) with
    | true -> Rgba8Bit.TransparentColor
    | false ->
        let flatColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy
        let verticalColor = Rgba8Bit.rgbaColor 0uy 0uy 0uy 255uy 

        let degrees = radToDeg slope

        Rgba8Bit.mixColors flatColor verticalColor (degrees / 90.)