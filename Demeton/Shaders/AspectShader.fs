[<RequireQualifiedAccess>]
module Demeton.Shaders.AspectShader

open Demeton.Geometry.Common
open Demeton.Shaders.Hillshading
open Png

open System

let run: PixelHillshader = fun _  _  _ aspect ->
    let mixColors colorA colorB degrees minDegrees =
        Rgba8Bit.mixColors 
            colorA colorB ((degrees - minDegrees) / 90.)

    match Double.IsNaN(aspect) with
    | true -> Rgba8Bit.TransparentColor
    | false ->
        let northColor = Rgba8Bit.rgbColor 255uy 0uy 0uy
        let eastColor = Rgba8Bit.rgbColor 0uy 255uy 0uy
        let southColor = Rgba8Bit.rgbColor 0uy 0uy 255uy
        let westColor = Rgba8Bit.rgbColor 0uy 255uy 255uy

        match radToDeg aspect with
        | x when x <= 90. -> mixColors northColor eastColor x 0.
        | x when x <= 180. -> mixColors eastColor southColor x 90.
        | x when x <= 270. -> mixColors southColor westColor x 180.
        | x -> mixColors westColor northColor x 270.
