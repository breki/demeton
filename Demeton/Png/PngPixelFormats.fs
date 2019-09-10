module Demeton.PngPixelFormats

open Demeton.PngTypes

/// <summary>
/// Generates a sequence of scanlines from the specified 8-bit grayscale image
/// data.
/// </summary>
/// <param name="imageData">The image data to generate scanlines from.</param>
/// <returns>A sequence of scanlines.</returns>
let grayscale8BitScanlines (imageData: Grayscale8BitImageData): Scanline seq =
    seq {
        for y in 0..(Array2D.length2 imageData - 1) do
            yield imageData.[0..(Array2D.length1 imageData - 1),y]
    }


let scanlinesToGrayscale8Bit 
    imageWidth
    imageHeight
    (scanlines: Scanline[]): Grayscale8BitImageData =
    Array2D.init imageWidth imageHeight (fun x y -> scanlines.[y].[x])

