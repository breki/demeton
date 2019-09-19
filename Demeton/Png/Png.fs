module Demeton.Png

open Demeton.PngTypes
open Demeton.PngStructure
open Demeton.PngChunks

open System.IO

/// <summary>
/// Saves the specified 8-bit grayscale image to a stream.
/// </summary>
/// <param name="imageData">The data of the image to be saved.</param>
/// <param name="stream">The stream the image should be written to.</param>
/// <returns>The same stream.</returns>
let savePngToStream 
    (ihdr: IhdrData)
    (imageData: ImageData) 
    (stream: Stream): Stream =

    let bpp = 
        match (ihdr.ColorType, ihdr.BitDepth) with
        | (PngColorType.Grayscale, PngBitDepth.BitDepth8) -> 8
        | (PngColorType.Grayscale, PngBitDepth.BitDepth16) -> 16
        | (_, _) -> 
            invalidOp "This PNG type is currently not supported."

    stream 
    |> writeSignature 
    |> writeIhdrChunk ihdr
    |> writeIdatChunk ihdr.Width ihdr.Height ihdr.BitsPerPixel imageData
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
let loadPngFromStream (stream: Stream) : (IhdrData * ImageData) =
    
    stream |> readSignature |> ignore
    let ihdr = stream |> readIhdrChunk
    let imageWidth = ihdr.Width
    let imageHeight = ihdr.Height

    let (chunkType, chunkData) = stream |> readChunk

    match chunkType.TypeName with
    | "IDAT" -> 
        let imageData = 
            deserializeIdatChunkData 
                ihdr.BitsPerPixel imageWidth imageHeight chunkData

        (ihdr, imageData)
    | x -> invalidOp 
                (sprintf "Expected IDAT PNG chunk, but got %s." x)
