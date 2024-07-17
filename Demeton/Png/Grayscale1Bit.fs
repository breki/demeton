[<RequireQualifiedAccess>]
module Png.Grayscale1Bit

open Raster

type ImageDataInitializer = ImageDataInitializer2D of (int -> int -> bool)

/// <summary>
/// Create a 1-bit grayscale raw image data array using the specified
/// initializer.
/// </summary>
let createImageData
    imageWidth
    imageHeight
    (initializer: ImageDataInitializer)
    : RawImageData =

    let rowDataSizeInBytes = (imageWidth + 7) / 8
    let imageDataSizeInBytes = rowDataSizeInBytes * imageHeight
    let imageData = Array.zeroCreate imageDataSizeInBytes
    let mutable byteIndex = 0

    match initializer with
    | ImageDataInitializer2D initializer2D ->
        for y in 0 .. imageHeight - 1 do

            let mutable bitIndex = 7

            for x in 0 .. imageWidth - 1 do
                let pixelValue = initializer2D x y

                if pixelValue then
                    imageData.[byteIndex] <-
                        imageData.[byteIndex] ||| (1uy <<< bitIndex)

                bitIndex <- bitIndex - 1

                if x = imageWidth - 1 then
                    byteIndex <- byteIndex + 1
                else if bitIndex < 0 then
                    bitIndex <- 7
                    byteIndex <- byteIndex + 1

    imageData


/// <summary>
/// Get the 1-bit grayscale pixel value at the specified coordinates.
/// </summary>
let pixelAt (imageData: byte[]) imageWidth x y : bool =
    let rowDataSizeInBytes = (imageWidth + 7) / 8
    let byteIndex = x / 8 + y * rowDataSizeInBytes
    let bitIndex = 7 - x % 8
    let mask = 1uy <<< bitIndex
    (imageData.[byteIndex] &&& mask) <> 0uy
