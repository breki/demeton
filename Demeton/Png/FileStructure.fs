﻿module Png.FileStructure

open System.Text
open Png.Types

open System.IO

let pngSignature =
    [| 0x89uy; 0x50uy; 0x4euy; 0x47uy; 0x0duy; 0x0auy; 0x1auy; 0x0auy |]

/// <summary>Writes the 8-byte PNG signature to a stream.</summary>
/// <param name="stream">The stream the signature should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeSignature (stream: Stream) : Stream =
    pngSignature |> Array.iter stream.WriteByte
    stream


let readSignature (stream: Stream) : Stream =
    let signatureLength = pngSignature.Length

    let signatureRead =
        [| for _ in 0 .. (signatureLength - 1) -> Bnry.readByte stream |]

    if signatureRead = pngSignature then
        stream
    else
        invalidOp "Invalid PNG signature"


/// <summary>
/// Writes the specified 4-characters PNG chunk type string to a stream.
//// </summary>
/// <param name="chunkType">The chunk type string to be written.</param>
/// <param name="stream">The stream the chunk type should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeChunkType (chunkType: ChunkType) (stream: Stream) : Stream =
    for i in 0 .. chunkType.TypeName.Length - 1 do
        stream |> Bnry.writeByte (byte chunkType.TypeName.[i]) |> ignore

    stream


let readChunkType (expectedChunkType: ChunkType) (stream: Stream) : Stream =
    let typeName = expectedChunkType.TypeName

    for i in 0 .. typeName.Length - 1 do
        let expectedChar = typeName.[i]
        let actualChar = char (Bnry.readByte stream)

        if actualChar = expectedChar then
            ignore ()
        else
            invalidOp (
                sprintf "Unexpected PNG chunk type read (expected %s)." typeName
            )

    stream



let writeChunk (chunkTypeAndDataBytes: byte[]) (stream: Stream) : Stream =
    let chunkDataLength = chunkTypeAndDataBytes.Length - 4

    let chunkCrc = Crc.crc32 chunkTypeAndDataBytes

    stream
    |> Bnry.writeBigEndianInt32 chunkDataLength
    |> Bnry.writeBytes chunkTypeAndDataBytes
    |> Bnry.writeBigEndianUInt32 chunkCrc

/// <summary>
/// Reads a PNG chunk from the provided stream and returns the chunk type along
/// with its data.
/// </summary>
/// <param name="stream">
/// The stream from which to read the PNG chunk.</param>
/// <returns>
/// A tuple containing the chunk type as <see cref="ChunkType"/> and the chunk
/// data as a byte array.
/// The chunk type is determined from the first 4 bytes of the chunk,
/// representing the chunk's type code.
/// The chunk data includes the chunk type code and the data bytes following it,
/// up to the length specified in the chunk.
/// </returns>
/// <exception cref="System.InvalidOperationException">
/// Throws an InvalidOperationException if the CRC check fails, indicating
/// the chunk data may be corrupted.
/// </exception>
let readChunk (stream: Stream) : ChunkType * byte[] =
    let chunkDataLength = stream |> Bnry.readBigEndianInt32

    let bufferLength = 10 * 1024 * 1024
    // '4' is for the chunk type
    let chunkTypeAndDataBytes =
        stream |> Bnry.readBytes bufferLength (4 + chunkDataLength)

    let chunkCrc = stream |> Bnry.readBigEndianUInt32

    let chunkTypeInBytes = chunkTypeAndDataBytes |> Array.take 4

    let chunkType = ChunkType(ASCIIEncoding.ASCII.GetString(chunkTypeInBytes))

    let expectedChunkCrc = Crc.crc32 chunkTypeAndDataBytes

    if chunkCrc <> expectedChunkCrc then
        invalidOp $"Wrong CRC for PNG chunk type %A{chunkType}."
    else
        (chunkType, chunkTypeAndDataBytes)
