module Png.PixelFormats

open Png.Types

type Grayscale16BitImageDataInitializer =
    Grayscale16BitImageDataInitializer1D of (int -> uint16)
    | Grayscale16BitImageDataInitializer2D of (int -> int -> uint16)

let grayscale16BitImageData 
    imageWidth 
    imageHeight 
    (initializer: Grayscale16BitImageDataInitializer): RawImageData =

    let imageDataSizeInBytes = imageWidth * imageHeight * 2
    let imageData = Array.zeroCreate imageDataSizeInBytes
    let mutable byteIndex = 0

    match initializer with
    | Grayscale16BitImageDataInitializer2D initializer2D ->
        for y in 0 .. imageHeight - 1 do
            for x in 0 .. imageWidth - 1 do
                let pixelValue = initializer2D x y

                imageData.[byteIndex] <- byte (pixelValue >>> 8)
                imageData.[byteIndex+1] <- byte pixelValue
                byteIndex <- byteIndex + 2

    | Grayscale16BitImageDataInitializer1D initializer1D ->
        let mutable pixelIndex = 0
        while byteIndex < imageDataSizeInBytes do
            let pixelValue = initializer1D pixelIndex

            imageData.[byteIndex] <- byte (pixelValue >>> 8)
            imageData.[byteIndex+1] <- byte pixelValue
            byteIndex <- byteIndex + 2

    imageData


let grayscale16BitPixel 
    (imageData: byte[])
    imageWidth
    imageHeight
    x
    y
    : uint16 =
    let pixelIndex = x * 2 + y * imageWidth
    let highByte = uint16 imageData.[pixelIndex]
    let lowByte = uint16 imageData.[pixelIndex + 1]
    highByte <<< 8 ||| lowByte
