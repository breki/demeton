module Demeton.PngPixelFormats

open Demeton.PngTypes
open Demeton.Png
open Demeton.PngChunks

open System.IO

/// <summary>
/// Generates an array of scanlines from the specified 8-bit grayscale image
/// data.
/// </summary>
/// <param name="imageData">The image data to generate scanlines from.</param>
/// <returns>An array of scanlines.</returns>
let grayscale8BitScanlines (imageData: Grayscale8BitImageData): Scanline[] =
    let imageHeight = Array2D.length2 imageData
    Array.init 
        imageHeight 
        (fun y -> imageData.[0..(Array2D.length1 imageData - 1),y])


let scanlinesToGrayscale8Bit 
    imageWidth
    imageHeight
    (scanlines: Scanline[]): Grayscale8BitImageData =
    Array2D.init imageWidth imageHeight (fun x y -> scanlines.[y].[x])


/// <summary>
/// Generates an array of scanlines from the specified 16-bit grayscale image
/// data.
/// </summary>
/// <param name="imageData">The image data to generate scanlines from.</param>
/// <returns>An array of scanlines.</returns>
let grayscale16BitScanlines (imageData: Grayscale16BitImageData): Scanline [] =
    let imageHeight = Array2D.length2 imageData

    let generateScanline y =
        [| 
            for x in 0..(Array2D.length1 imageData - 1) do
                let pixelData = imageData.[x, y]
                yield ((byte)(pixelData >>> 8))
                yield ((byte)pixelData)
        |]

    Array.init imageHeight generateScanline


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
    let scanlines = grayscale8BitScanlines imageData

    stream 
    |> writeSignature 
    |> writeIhdrChunk ihdr
    |> writeIdatChunk bpp scanlines
    |> writeIendChunk


/// <summary>
/// Saves the specified 16-bit grayscale image to a stream.
/// </summary>
/// <param name="imageData">The data of the image to be saved.</param>
/// <param name="stream">The stream the image should be written to.</param>
/// <returns>The same stream.</returns>
let saveGrayscale16BitToStream 
    (imageData: Grayscale16BitImageData) 
    (stream: Stream): Stream =

    let imageWidth = Array2D.length1 imageData
    let imageHeight = Array2D.length2 imageData
    let bpp = 16
    let ihdr = 
        { Width = imageWidth; Height = imageHeight; 
            BitDepth = PngBitDepth.BitDepth16; 
            ColorType = PngColorType.Grayscale; 
            InterlaceMethod = PngInterlaceMethod.NoInterlace }
    let scanlines = grayscale16BitScanlines imageData |> Seq.toArray

    stream 
    |> writeSignature 
    |> writeIhdrChunk ihdr
    |> writeIdatChunk bpp scanlines
    |> writeIendChunk


/// <summary>
/// Decodes a PNG image from a stream.
/// </summary>
/// <param name="onGrayscale8BitLoad">
/// Function that is called when a 8-bit grayscale image data is decoded.
/// The image data is provided as the parameter of the call. 
/// </param>
/// <param name="onGrayscale16BitLoad">
/// Function that is called when a 16-bit grayscale image data is decoded.
/// The image data is provided as the parameter of the call. 
/// </param>
/// <param name="stream">The stream containing the PNG image.</param>
/// <returns>The same stream.</returns>
let loadPngFromStream 
    onGrayscale8BitLoad
    onGrayscale16BitLoad
    (stream: Stream): Stream =
    
    stream |> readSignature |> ignore
    let ihdr = stream |> readIhdrChunk
    let imageWidth = ihdr.Width
    let imageHeight = ihdr.Height

    match (ihdr.ColorType, ihdr.BitDepth) with
    | (PngColorType.Grayscale, PngBitDepth.BitDepth8) ->
        let (chunkType, chunkData) = stream |> readChunk
        match chunkType.TypeName with
        | "IDAT" -> 
            let scanlinesRead = 
                deserializeIdatChunkData 8 imageWidth chunkData
            let imageDataRead = 
                scanlinesToGrayscale8Bit 
                    imageWidth imageHeight scanlinesRead
            onGrayscale8BitLoad imageDataRead
            stream
        | x -> invalidOp 
                    (sprintf "Expected IDAT PNG chunk, but got %s." x)

    | (PngColorType.Grayscale, PngBitDepth.BitDepth16) ->
        let (chunkType, chunkData) = stream |> readChunk
        match chunkType.TypeName with
        | "IDAT" -> 
            let scanlinesRead = 
                deserializeIdatChunkData 16 imageWidth chunkData
            let imageDataRead = 
                scanlinesToGrayscale16Bit 
                    imageWidth imageHeight scanlinesRead
            onGrayscale16BitLoad imageDataRead
            stream
        | x -> invalidOp 
                    (sprintf "Expected IDAT PNG chunk, but got %s." x)

    | (_, _) -> 
        invalidOp "This PNG type is currently not supported."
