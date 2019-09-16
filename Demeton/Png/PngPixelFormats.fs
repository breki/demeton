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
let grayscale8BitScanlines (imageData: Grayscale8BitImageData)
    : (ImageData * Scanline[]) =
    let imageWidth = Array2D.length1 imageData
    let imageHeight = Array2D.length2 imageData

    let imageData2DTo1D i =
        byte (imageData.[i % imageWidth, i / imageWidth])

    let rawImageData: ImageData = 
        Array.Parallel.init (imageWidth * imageHeight) imageData2DTo1D

    let scanlines =
        Array.Parallel.init 
            imageHeight 
            (fun y -> imageData.[0..(Array2D.length1 imageData - 1),y])

    (rawImageData, scanlines)


let scanlinesToGrayscale8Bit 
    imageWidth
    imageHeight
    (imageData: byte[]): Grayscale8BitImageData =
    Array2D.init 
        imageWidth 
        imageHeight 
        (fun x y -> imageData.[y * imageWidth + x])


/// <summary>
/// Generates an array of scanlines from the specified 16-bit grayscale image
/// data.
/// </summary>
/// <param name="imageData">The image data to generate scanlines from.</param>
/// <returns>An array of scanlines.</returns>
let grayscale16BitScanlines (imageData: Grayscale16BitImageData)
    : (ImageData * Scanline []) =
    let imageWidth = Array2D.length1 imageData
    let imageHeight = Array2D.length2 imageData

    let imageData2DTo1D i =
        let x = i / 2 % imageWidth
        let y = i / (imageWidth * 2)
        let pixelValue = imageData.[x, y]

        match i % 2 with
        | 0 -> byte (pixelValue >>> 8)
        | _ -> byte pixelValue

    let rawImageData: ImageData =
        Array.Parallel.init 
            (imageWidth * imageHeight * 2)
            imageData2DTo1D

    let inline generateScanline y =
        let scanline: Scanline = Array.zeroCreate (imageWidth * 2)

        for x in 0 .. (imageWidth-1) do
            let bx = x <<< 1
            let pixelData = imageData.[x, y]
            scanline.[bx] <- byte (pixelData >>> 8)
            scanline.[bx + 1] <- byte pixelData

        scanline

    let scanlines = Array.Parallel.init imageHeight generateScanline

    (rawImageData, scanlines)


let scanlinesToGrayscale16Bit 
    imageWidth
    imageHeight
    (imageData: ImageData): Grayscale16BitImageData =

    let inline pixelFromImageData x y =
        let pixelIndex = y * imageWidth * 2 + x * 2
        let highByte = imageData.[pixelIndex]
        let lowByte = imageData.[pixelIndex + 1]
        ((uint16)highByte) <<< 8 ||| (uint16)lowByte

    Array2D.init 
        imageWidth imageHeight (fun x y -> pixelFromImageData x y)


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
    let (rawImageData, _) = grayscale8BitScanlines imageData

    stream 
    |> writeSignature 
    |> writeIhdrChunk ihdr
    |> writeIdatChunk imageWidth imageHeight bpp rawImageData
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
    let (rawImageData, _) = grayscale16BitScanlines imageData

    stream 
    |> writeSignature 
    |> writeIhdrChunk ihdr
    |> writeIdatChunk imageWidth imageHeight bpp rawImageData
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
            let imageData = 
                deserializeIdatChunkData 8 imageWidth imageHeight chunkData
            let imageDataRead = 
                scanlinesToGrayscale8Bit imageWidth imageHeight imageData
            onGrayscale8BitLoad imageDataRead
            stream
        | x -> invalidOp 
                    (sprintf "Expected IDAT PNG chunk, but got %s." x)

    | (PngColorType.Grayscale, PngBitDepth.BitDepth16) ->
        let (chunkType, chunkData) = stream |> readChunk
        match chunkType.TypeName with
        | "IDAT" -> 
            let imageData = 
                deserializeIdatChunkData 16 imageWidth imageHeight chunkData
            let imageDataRead = 
                scanlinesToGrayscale16Bit imageWidth imageHeight imageData
            onGrayscale16BitLoad imageDataRead
            stream
        | x -> invalidOp 
                    (sprintf "Expected IDAT PNG chunk, but got %s." x)

    | (_, _) -> 
        invalidOp "This PNG type is currently not supported."
