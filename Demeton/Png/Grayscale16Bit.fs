[<RequireQualifiedAccess>]
module Png.Grayscale16Bit

open Raster

type ImageDataInitializer =
    | ImageDataInitializer1D of (int -> uint16)
    | ImageDataInitializer2D of (int -> int -> uint16)

/// <summary>
/// Create a 16-bit grayscale raw image data array using the specified
/// initializer.
/// </summary>
let createImageData
    imageWidth
    imageHeight
    (initializer: ImageDataInitializer)
    : RawImageData =

    let imageDataSizeInBytes = imageWidth * imageHeight * 2
    let imageData = Array.zeroCreate imageDataSizeInBytes
    let mutable byteIndex = 0

    match initializer with
    | ImageDataInitializer2D initializer2D ->
        for y in 0 .. imageHeight - 1 do
            for x in 0 .. imageWidth - 1 do
                let pixelValue = initializer2D x y

                imageData.[byteIndex] <- byte (pixelValue >>> 8)
                imageData.[byteIndex + 1] <- byte pixelValue
                byteIndex <- byteIndex + 2

    | ImageDataInitializer1D initializer1D ->
        let mutable pixelIndex = 0

        while byteIndex < imageDataSizeInBytes do
            let pixelValue = initializer1D pixelIndex

            imageData.[byteIndex] <- byte (pixelValue >>> 8)
            imageData.[byteIndex + 1] <- byte pixelValue
            byteIndex <- byteIndex + 2
            pixelIndex <- pixelIndex + 1

    imageData


/// <summary>
/// Get the 16-bit grayscale pixel value at the specified coordinates.
/// </summary>
let pixelAt (imageData: byte[]) imageWidth x y : uint16 =
    let pixelIndex = x * 2 + y * imageWidth * 2
    let highByte = uint16 imageData.[pixelIndex]
    let lowByte = uint16 imageData.[pixelIndex + 1]
    highByte <<< 8 ||| lowByte
