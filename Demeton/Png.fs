module Demeton.Png

open Demeton.PngTypes

open System
open System.IO
open ICSharpCode.SharpZipLib.Zip.Compression

/// <summary>Writes the 8-byte PNG signature to a stream.</summary>
/// <param name="stream">The stream the signature should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeSignature (stream: Stream): Stream =
    let pngSignature = [| 0x89uy; 0x50uy; 0x4euy; 0x47uy; 0x0duy; 0x0auy; 
                                0x1auy; 0x0auy |]
    Array.ForEach(pngSignature, (fun x -> stream.WriteByte x))
    stream

/// <summary>Writes the specified byte value to a stream.</summary>
/// <param name="value">The byte value to be written.</param>
/// <param name="stream">The stream the byte value should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeByte (value: byte) (stream: Stream): Stream =
    stream.WriteByte(value)
    stream


let readByte (stream: Stream) =
    let read = stream.ReadByte()
    match read with
    | -1 -> invalidOp "Unexpected EOF reached in the stream."
    | _ -> (byte)read

/// <summary>Writes the specified byte array to a stream.</summary>
/// <param name="value">The byte array to be written.</param>
/// <param name="stream">The stream the byte array should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeBytes (bytes: byte[]) (stream: Stream): Stream =
    stream.Write (bytes, 0, bytes.Length)
    stream


/// <summary>
/// Writes the specified integer value to a stream using the big endian order.
//// </summary>
/// <param name="value">The integer value to be written.</param>
/// <param name="stream">The stream the integer value should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeBigEndianInt32 (value: int) (stream: Stream): Stream =
    stream
    |> writeByte ((byte)(value >>> 24))
    |> writeByte ((byte)(value >>> 16))
    |> writeByte ((byte)(value >>> 8))
    |> writeByte ((byte)value)


let readBigEndianInt32 (stream: Stream): int =
    (((int)(readByte stream)) <<< 24)
    ||| (((int)(readByte stream)) <<< 16)
    ||| (((int)(readByte stream)) <<< 8)
    ||| (((int)(readByte stream)))

/// <summary>
/// Writes the specified unsigned integer value to a stream using the big 
/// endian order.
//// </summary>
/// <param name="value">The unsigned integer value to be written.</param>
/// <param name="stream">
/// The stream the unsigned integer value should be written to.
/// </param>
/// <returns>The same instance of the stream.</returns>
let writeBigEndianUInt32 (value: uint32) (stream: Stream): Stream =
    stream
    |> writeByte ((byte)(value >>> 24))
    |> writeByte ((byte)(value >>> 16))
    |> writeByte ((byte)(value >>> 8))
    |> writeByte ((byte)value)


/// <summary>
/// Writes the specified 4-characters PNG chunk type string to a stream.
//// </summary>
/// <param name="chunkType">The chunk type string to be written.</param>
/// <param name="stream">The stream the chunk type should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeChunkType (chunkType: ChunkType) (stream: Stream): Stream = 
    for i in 0 .. chunkType.TypeName.Length - 1 do
        stream |> writeByte ((byte) chunkType.TypeName.[i]) |> ignore

    stream


let readChunkType (expectedChunkType: ChunkType) (stream: Stream): Stream = 
    let typeName = expectedChunkType.TypeName

    for i in 0 .. typeName.Length - 1 do
        let expectedChar = typeName.[i]
        let actualChar = (char) (readByte stream)
        if actualChar = expectedChar then ignore()
        else
            invalidOp (sprintf "Unexpected PNG chunk type read (expected %s)." typeName)

    stream

/// <summary>
/// Serializes the PNG IHDR chunk type and data into a byte array.
//// </summary>
/// <param name="ihdr">
/// <see cref="IhdrData"/>value containing the IHDR chunk data.
/// </param>
/// <returns>
/// The byte array containing the PNG IHDR chunk type and data.
/// </returns>
let serializeIhdrChunkData (ihdr: IhdrData): byte[] =
    use stream = new MemoryStream()

    stream
    |> writeChunkType (new ChunkType("IHDR"))
    |> writeBigEndianInt32 ihdr.Width
    |> writeBigEndianInt32 ihdr.Height
    |> writeByte ((byte)ihdr.BitDepth)
    |> writeByte ((byte)ihdr.ColorType)
    |> writeByte ((byte)PngCompressionMethod.DeflateInflate)
    |> writeByte ((byte)PngFilterMethod.AdaptiveFiltering)
    |> writeByte ((byte)ihdr.InterlaceMethod)
    |> ignore

    stream.ToArray()


let deserializeIhdrChunkData (bytes: byte[]) =
    let assertNextByteIs expected stream =
        let nextByte = readByte stream
        match nextByte with
        | expected -> stream
        | _ -> invalidOp (sprintf "Expected byte is %O but was %O." expected nextByte)

    use stream = new MemoryStream(bytes)

    stream 
    |> readChunkType (new ChunkType("IHDR"))
    |> ignore

    { 
        Width = readBigEndianInt32 stream;
        Height = readBigEndianInt32 stream;
        BitDepth = LanguagePrimitives.EnumOfValue (readByte stream);
        ColorType = LanguagePrimitives.EnumOfValue (readByte stream);
        InterlaceMethod = 
            stream 
            |> assertNextByteIs ((byte)PngCompressionMethod.DeflateInflate)
            |> assertNextByteIs ((byte)PngFilterMethod.AdaptiveFiltering)
            |> readByte 
            |> LanguagePrimitives.EnumOfValue 
    }


/// <summary>
/// Serializes the PNG IEND chunk type and data into a byte array.
//// </summary>
/// <param name="ihdr">
/// <see cref="IhdrData"/>value containing the IEND chunk data.
/// </param>
/// <returns>
/// The byte array containing the PNG IEND chunk type and data.
/// </returns>
let serializeIendChunkData(): byte[] =
    use stream = new MemoryStream()

    stream
    |> writeChunkType (new ChunkType("IEND"))
    |> ignore

    stream.ToArray()


let writeChunk 
    (chunkDataWriter: ChunkDataWriter) 
    (stream: Stream)
    : Stream =
    let chunkTypeAndDataBytes = chunkDataWriter()
    let chunkDataLength = chunkTypeAndDataBytes.Length - 4

    let chunkCrc = CRC.crc32 chunkTypeAndDataBytes

    stream
    |> writeBigEndianInt32 chunkDataLength
    |> writeBytes chunkTypeAndDataBytes
    |> writeBigEndianUInt32 chunkCrc

let writeIhdrChunk (ihdr: IhdrData) (stream: Stream): Stream =
    stream |> writeChunk (fun () -> serializeIhdrChunkData (ihdr))


let compress data (outputStream: Stream) : unit =
    let deflater = new Deflater()
    deflater.SetInput(data)
    deflater.Finish()

    while not deflater.IsFinished do
        let compressionBuffer: byte[] = Array.zeroCreate (100 * 1024 * 4)

        let producedBytesCount = deflater.Deflate(compressionBuffer)
        outputStream.Write(compressionBuffer, 0, producedBytesCount)


let decompress compressedData (outputStream: Stream) : unit =
    let inflater = new Inflater()
    inflater.SetInput(compressedData)

    while not inflater.IsFinished do
        let inflaterBuffer: byte[] = Array.zeroCreate (100 * 1024 * 4)
        let producedBytesCount = inflater.Inflate(inflaterBuffer)
        outputStream.Write(inflaterBuffer, 0, producedBytesCount)

let writeIdatChunk (stream: Stream): Stream =
    // The sequence of filtered scanlines is compressed and the resulting data 
    // stream is split into IDAT chunks. The concatenation of the contents of 
    // all the IDAT chunks makes up a zlib datastream. This datastream 
    // decompresses to filtered image data.

    // Filtering transforms the byte sequence in a scanline to an equal length 
    // sequence of bytes preceded by the filter type.
    stream


let writeIendChunk (stream: Stream): Stream =
    stream


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


