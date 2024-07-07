[<RequireQualifiedAccess>]
module Png.AlphaCompositing

open Raster

open System
open System.Threading.Tasks

type ColorRgbRatio = float * float * float

let inline byteToRatio (byteValue: byte) = (float byteValue) / 255.

let ratioToByte (ratio: float) : byte =
    let asInt = int (Math.Round(ratio * 255.))
    byte (min (max asInt 0) 255)

let toPremultiplied color : ColorRgbRatio =
    let inline premultiply value a = float (int value * a) / 65025.

    let a = int (Rgba8Bit.a color)
    let rPremultiplied = premultiply (Rgba8Bit.r color) a
    let gPremultiplied = premultiply (Rgba8Bit.g color) a
    let bPremultiplied = premultiply (Rgba8Bit.b color) a
    (rPremultiplied, gPremultiplied, bPremultiplied)

let toRgbaColor ((r, g, b): ColorRgbRatio) (a: float) : Rgba8Bit.RgbaColor =
    let inline fromPremultiplied value a = ratioToByte (value / a)

    Rgba8Bit.rgbaColor
        (fromPremultiplied r a)
        (fromPremultiplied g a)
        (fromPremultiplied b a)
        (ratioToByte a)

let pixelOver source dest =
    let inline addColors
        ((r1, g1, b1): ColorRgbRatio)
        ((r2, g2, b2): ColorRgbRatio)
        : ColorRgbRatio =
        ((r1 + r2), (g1 + g2), (b1 + b2))

    let inline multiplyColor factor ((r, g, b): ColorRgbRatio) : ColorRgbRatio =
        (r * factor, g * factor, b * factor)

    let sourceAb = Rgba8Bit.a source
    let destAb = Rgba8Bit.a dest

    match (sourceAb, destAb) with
    | 0uy, _ -> dest
    | 255uy, _ -> source
    | _, 0uy -> source
    | _, 255uy ->
        let sourceAr = byteToRatio sourceAb
        let srcRgbR = toPremultiplied source
        let destRgbR = toPremultiplied dest

        let outR = addColors srcRgbR (multiplyColor (1. - sourceAr) destRgbR)
        let outA = 1.

        toRgbaColor outR outA
    | _ ->
        let sourceAr = byteToRatio sourceAb
        let destAr = byteToRatio destAb

        let srcRgbR = toPremultiplied source
        let destRgbR = toPremultiplied dest

        let outR = addColors srcRgbR (multiplyColor (1. - sourceAr) destRgbR)
        let outA = sourceAr + destAr * (1. - sourceAr)

        toRgbaColor outR outA

type CompositingFunc =
    int -> int -> RawImageData -> RawImageData -> RawImageData

let imageOver: CompositingFunc =
    fun
        (imageWidth: int)
        (imageHeight: int)
        (source: RawImageData)
        (dest: RawImageData) ->
        Parallel.For(
            0,
            imageHeight,
            fun y ->
                for x in 0 .. imageWidth - 1 do
                    let sourcePixel = Rgba8Bit.pixelAt source imageWidth x y
                    let destPixel = Rgba8Bit.pixelAt dest imageWidth x y

                    let outPixel = pixelOver sourcePixel destPixel
                    Rgba8Bit.setPixelAt dest imageWidth x y outPixel
        )
        |> ignore

        dest

let darken: CompositingFunc =
    fun
        (imageWidth: int)
        (imageHeight: int)
        (source: RawImageData)
        (dest: RawImageData) ->
        Parallel.For(
            0,
            imageHeight,
            fun y ->
                for x in 0 .. imageWidth - 1 do
                    let sourceDarknessRatio =
                        Rgba8Bit.pixelAt source imageWidth x y
                        |> Rgba8Bit.a
                        |> byteToRatio

                    let destDarknessRatio =
                        Rgba8Bit.pixelAt dest imageWidth x y
                        |> Rgba8Bit.a
                        |> byteToRatio

                    let outDarknessRatio =
                        1.
                        - ((1. - sourceDarknessRatio) * (1. - destDarknessRatio))

                    let outAlpha = ratioToByte outDarknessRatio

                    // todo sometime 5: use source RGB value instead of always black
                    let outPixel = Rgba8Bit.rgbaColor 0uy 0uy 0uy outAlpha

                    Rgba8Bit.setPixelAt dest imageWidth x y outPixel
        )
        |> ignore

        dest
