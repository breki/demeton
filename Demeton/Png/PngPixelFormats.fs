module Demeton.PngPixelFormats

open Demeton.PngTypes
open Demeton.Png
open Demeton.PngChunks

open System.IO

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


/// <summary>
/// Generates a sequence of scanlines from the specified 16-bit grayscale image
/// data.
/// </summary>
/// <param name="imageData">The image data to generate scanlines from.</param>
/// <returns>A sequence of scanlines.</returns>
let grayscale16BitScanlines (imageData: Grayscale16BitImageData): Scanline seq =
    seq {
        for y in 0..(Array2D.length2 imageData - 1) ->
        [| 
            for x in 0..(Array2D.length1 imageData - 1) do
                let pixelData = imageData.[x, y]
                yield ((byte)(pixelData >>> 8))
                yield ((byte)pixelData)
        |]
    }


let scanlinesToGrayscale16Bit 
    imageWidth
    imageHeight
    (scanlines: Scanline[]): Grayscale16BitImageData =

    let pixelFromScanline (x: int) (scanline: Scanline) =
        let highByte = scanline.[x * 2]
        let lowByte = scanline.[x * 2 + 1]
        ((uint16)highByte) <<< 8 ||| (uint16)lowByte

    Array2D.init 
        imageWidth imageHeight (fun x y -> pixelFromScanline x scanlines.[y])


/// <summary>
/// Saves the specified 8-bit grayscale image to a stream.
/// </summary>
/// <param name="imageData">The data of the image to be saved.</param>
/// <param name="stream">The stream the image should be written to.</param>
/// <returns>The same stream.</returns>
let saveGrayscale8BitToStream 
    (imageData: Grayscale8BitImageData) 
    (stream: Stream): Stream =

    let imageWidth = Array2D.length1 imageData
    let imageHeight = Array2D.length2 imageData
    let bpp = 8
    let ihdr = 
        { Width = imageWidth; Height = imageHeight; 
            BitDepth = PngBitDepth.BitDepth8; 
            ColorType = PngColorType.Grayscale; 
            InterlaceMethod = PngInterlaceMethod.NoInterlace }
    let scanlines = grayscale8BitScanlines imageData |> Seq.toArray

    stream 
    |> writeSignature 
    |> writeIhdrChunk ihdr
    |> writeIdatChunk bpp scanlines
    |> writeIendChunk
