module Demeton.PngPixelFormats

open Demeton.PngTypes


let grayscale16BitImageData 
    imageWidth 
    imageHeight 
    (initializer: int -> int -> uint16): ImageData =

    let imageData = Array.zeroCreate (imageWidth * imageHeight * 2)

    let mutable byteIndex = 0
    for y in 0 .. imageHeight - 1 do
        for x in 0 .. imageWidth - 1 do
            let pixelValue = initializer x y

            imageData.[byteIndex] <- byte (pixelValue >>> 8)
            imageData.[byteIndex+1] <- byte pixelValue
            byteIndex <- byteIndex + 2

    imageData