[<RequireQualifiedAccess>]
module Png.AlphaCompositing

open Raster

open System

type private ColorRgbRatio = float * float * float

let pixelOver source dest = 
    let inline byteToRatio (byteValue: byte) = (float byteValue) / 255.

    let ratioToByte (ratio: float): byte = 
        let asInt = int (Math.Round (ratio * 255.))
        byte (min (max asInt 0) 255)

    let premultiply value a = float (int value * a) / 65025.

    let toPremultiplied color: ColorRgbRatio =
        let a = int (Rgba8Bit.a color)
        let rPremultiplied = premultiply (Rgba8Bit.r color) a
        let gPremultiplied = premultiply (Rgba8Bit.g color) a
        let bPremultiplied = premultiply (Rgba8Bit.b color) a
        (rPremultiplied, gPremultiplied, bPremultiplied)

    let fromPremultiplied value a = ratioToByte (255. * value / a)

    let inline addColors 
        ((r1, g1, b1): ColorRgbRatio) ((r2, g2, b2): ColorRgbRatio)
        : ColorRgbRatio =
        ((r1 + r2), (g1 + g2), (b1 + b2))

    let inline multiplyColor factor ((r, g, b): ColorRgbRatio): ColorRgbRatio =
        (r * factor, g * factor, b * factor)

    let toRgbaColor ((r, g, b): ColorRgbRatio) (a: float): Rgba8Bit.RgbaColor =       
        Rgba8Bit.rgbaColor
            (fromPremultiplied r a)
            (fromPremultiplied g a)
            (fromPremultiplied b a)
            (ratioToByte a)

    let sourceAb = Rgba8Bit.a source
    let destAb = Rgba8Bit.a dest

    match (sourceAb, destAb) with
    | (0uy, _) -> dest
    | (255uy, _) -> source
    | (_, 0uy) -> source
    | (_, 255uy) -> dest
    | _ -> 
        let sourceAr = byteToRatio sourceAb
        let destAr = byteToRatio destAb

        let srcRgbR = toPremultiplied source
        let destRgbR = toPremultiplied source

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
    for y in 0 .. imageHeight - 1 do
        for x in 0 .. imageWidth - 1 do
            let sourcePixel = Rgba8Bit.pixelAt source imageWidth x y
            let destPixel = Rgba8Bit.pixelAt dest imageWidth x y

            let outPixel = pixelOver sourcePixel destPixel
            Rgba8Bit.setPixelAt dest imageWidth x y outPixel

    dest
