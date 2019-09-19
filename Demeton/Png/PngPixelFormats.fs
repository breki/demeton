module Demeton.PngPixelFormats

open Demeton.PngTypes
open Demeton.PngStructure
open Demeton.PngChunks

open System.IO

// todo remove when no longer needed:

/// <summary>
/// Generates a byte array of raw image data from the specified 8-bit 
/// grayscale image data.
/// </summary>
/// <param name="imageData">The image data to generate scanlines from.</param>
/// <returns>A byte array of raw image data.</returns>
//let grayscale8BitRawImageData (imageData: Grayscale8BitImageData): ImageData =
//    let imageWidth = Array2D.length1 imageData
//    let imageHeight = Array2D.length2 imageData

//    let imageData2DTo1D i =
//        byte (imageData.[i % imageWidth, i / imageWidth])

//    Array.Parallel.init (imageWidth * imageHeight) imageData2DTo1D


//let rawImageDataToGrayscale8Bit 
//    imageWidth
//    imageHeight
//    (imageData: byte[]): Grayscale8BitImageData =
//    Array2D.init 
//        imageWidth 
//        imageHeight 
//        (fun x y -> imageData.[y * imageWidth + x])


/// <summary>
/// Generates an array of scanlines from the specified 16-bit grayscale image
/// data.
/// </summary>
/// <param name="imageData">The image data to generate scanlines from.</param>
/// <returns>An array of scanlines.</returns>
//let grayscale16BitRawImageData (imageData: Grayscale16BitImageData)
//    : ImageData =
//    let imageWidth = Array2D.length1 imageData
//    let imageHeight = Array2D.length2 imageData

//    let imageData2DTo1D i =
//        let x = i / 2 % imageWidth
//        let y = i / (imageWidth * 2)
//        let pixelValue = imageData.[x, y]

//        match i % 2 with
//        | 0 -> byte (pixelValue >>> 8)
//        | _ -> byte pixelValue
    
//    Array.Parallel.init (imageWidth * imageHeight * 2) imageData2DTo1D


//let rawImageDataToGrayscale16Bit 
//    imageWidth
//    imageHeight
//    (imageData: ImageData): Grayscale16BitImageData =

//    let inline pixelFromImageData x y =
//        let pixelIndex = y * imageWidth * 2 + x * 2
//        let highByte = imageData.[pixelIndex]
//        let lowByte = imageData.[pixelIndex + 1]
//        ((uint16)highByte) <<< 8 ||| (uint16)lowByte

//    Array2D.init 
//        imageWidth imageHeight (fun x y -> pixelFromImageData x y)


let grayscale16BitImageData 
    imageWidth 
    imageHeight 
    (initializer: int -> int -> uint16): ImageData =

    // todo: instead of initializer (which calculates the same pixel value) twice,
    // we should have for loops
    let initializerFlat i =
        let x = i / 2 % imageWidth
        let y = i / (imageWidth * 2)

        let pixelValue = initializer x y

        match i % 2 with
        | 0 -> byte (pixelValue >>> 8)
        | _ -> byte pixelValue

    Array.init 
        (imageWidth * imageHeight * 2)
        initializerFlat
