[<RequireQualifiedAccess>]
module Png.Rgba8Bit

open Png.Types

[<Literal>]
let BytesPerPixel = 4

type RgbaColor = uint32
type ArgbColor = uint32

let inline r(color: RgbaColor) = byte (color >>> 24)
let inline g(color: RgbaColor) = byte (color >>> 16)
let inline b(color: RgbaColor) = byte (color >>> 8)
let inline a(color: RgbaColor) = byte color

let inline rgbaColor (r: byte) (g: byte) (b: byte) (a: byte): RgbaColor =
    (uint32 r <<< 24) ||| (uint32 g <<< 16) ||| (uint32 b <<< 8) ||| (uint32 a)

let inline toArgb(color: RgbaColor): ArgbColor =
    (uint32 (a color) <<< 24) ||| (uint32 (r color) <<< 16)
    ||| (uint32 (g color) <<< 8) ||| (uint32 (b color))

let inline toHex(color: RgbaColor): string =
    sprintf "%02X%02X%02X%02X" (a color) (r color) (g color) (b color)

type ImageDataInitializer =
    ImageDataInitializer1D of (int -> RgbaColor)
    | ImageDataInitializer2D of (int -> int -> RgbaColor)

let createImageData 
    imageWidth 
    imageHeight 
    (initializer: ImageDataInitializer): RawImageData =

    let imageDataSizeInBytes = imageWidth * imageHeight * BytesPerPixel
    let imageData = Array.zeroCreate imageDataSizeInBytes
    let mutable byteIndex = 0

    match initializer with
    | ImageDataInitializer2D initializer2D ->
        for y in 0 .. imageHeight - 1 do
            for x in 0 .. imageWidth - 1 do
                let pixelValue = initializer2D x y

                imageData.[byteIndex] <- r pixelValue
                imageData.[byteIndex+1] <- g pixelValue
                imageData.[byteIndex+2] <- b pixelValue
                imageData.[byteIndex+3] <- a pixelValue
                byteIndex <- byteIndex + BytesPerPixel

    | ImageDataInitializer1D initializer1D ->
        let mutable pixelIndex = 0
        while byteIndex < imageDataSizeInBytes do
            let pixelValue = initializer1D pixelIndex

            imageData.[byteIndex] <- r pixelValue
            imageData.[byteIndex+1] <- g pixelValue
            imageData.[byteIndex+2] <- b pixelValue
            imageData.[byteIndex+3] <- a pixelValue
            byteIndex <- byteIndex + BytesPerPixel
            pixelIndex <- pixelIndex + 1

    imageData


let pixelAt 
    (imageData: byte[])
    imageWidth
    x
    y
    : RgbaColor =
    let pixelIndex = x * BytesPerPixel + y * imageWidth * BytesPerPixel
    let r = imageData.[pixelIndex]
    let g = imageData.[pixelIndex+1]
    let b = imageData.[pixelIndex+2]
    let a = imageData.[pixelIndex+3]
    rgbaColor r g b a
