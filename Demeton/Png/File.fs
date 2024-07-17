module Png.File

open Raster
open Png.Types
open Png.FileStructure
open Png.Chunks

open System.IO

type PngStreamWriter = IhdrData -> RawImageData -> Stream -> Stream

/// <summary>
/// Saves the specified 8-bit grayscale image to a stream.
/// </summary>
/// <param name="ihdr">The IHDR chunk data.</param>
/// <param name="imageData">The data of the image to be saved.</param>
/// <param name="stream">The stream the image should be written to.</param>
/// <returns>The same stream.</returns>
let savePngToStream: PngStreamWriter =
    fun ihdr imageData stream ->

        stream
        |> writeSignature
        |> writeIhdrChunk ihdr
        |> writeIdatChunk ihdr.Width ihdr.Height ihdr.BitsPerPixel imageData
        |> writeIendChunk


/// <summary>
/// Decode a PNG image from a stream.
/// </summary>
/// <param name="stream">The stream containing the PNG image.</param>
/// <returns>A tuple of IHDR data and raw image data.</returns>
let loadPngFromStream (stream: Stream) : IhdrData * RawImageData =

    stream |> readSignature |> ignore
    let ihdr = stream |> readIhdrChunk
    let imageWidth = ihdr.Width
    let imageHeight = ihdr.Height

    let chunkType, chunkData = stream |> readChunk

    match chunkType.TypeName with
    | "IDAT" ->
        let imageData =
            deserializeIdatChunkData
                ihdr.BitsPerPixel
                imageWidth
                imageHeight
                chunkData

        (ihdr, imageData)
    | x -> invalidOp $"Expected IDAT PNG chunk, but got %s{x}."
