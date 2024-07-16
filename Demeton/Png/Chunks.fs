module Png.Chunks

open Raster
open Png.Types
open Png.Filters
open Png.Unfilters
open Png.FileStructure

open System.IO
open ICSharpCode.SharpZipLib.Zip.Compression

/// <summary>
/// Serializes the PNG IHDR chunk type and data into a byte array.
/// </summary>
/// <param name="ihdr">
/// <see cref="IhdrData"/>value containing the IHDR chunk data.
/// </param>
/// <returns>
/// The byte array containing the PNG IHDR chunk type and data.
/// </returns>
let serializeIhdrChunkData (ihdr: IhdrData) : byte[] =
    use stream = new MemoryStream()

    stream
    |> writeChunkType (ChunkType("IHDR"))
    |> Bnry.writeBigEndianInt32 ihdr.Width
    |> Bnry.writeBigEndianInt32 ihdr.Height
    |> Bnry.writeByte (byte ihdr.BitDepth)
    |> Bnry.writeByte (byte ihdr.ColorType)
    |> Bnry.writeByte (byte PngCompressionMethod.DeflateInflate)
    |> Bnry.writeByte (byte PngFilterMethod.AdaptiveFiltering)
    |> Bnry.writeByte (byte ihdr.InterlaceMethod)
    |> ignore

    stream.ToArray()


let deserializeIhdrChunkData (bytes: byte[]) =
    let assertNextByteIs expected stream =
        let nextByte = Bnry.readByte stream

        if nextByte = expected then
            stream
        else
            invalidOp (
                sprintf "Expected byte is %O but was %O." expected nextByte
            )

    use stream = new MemoryStream(bytes)

    stream |> readChunkType (ChunkType("IHDR")) |> ignore

    { Width = Bnry.readBigEndianInt32 stream
      Height = Bnry.readBigEndianInt32 stream
      BitDepth = LanguagePrimitives.EnumOfValue(Bnry.readByte stream)
      ColorType = LanguagePrimitives.EnumOfValue(Bnry.readByte stream)
      InterlaceMethod =
        stream
        |> assertNextByteIs (byte PngCompressionMethod.DeflateInflate)
        |> assertNextByteIs (byte PngFilterMethod.AdaptiveFiltering)
        |> Bnry.readByte
        |> LanguagePrimitives.EnumOfValue }


let writeIhdrChunk (ihdr: IhdrData) (stream: Stream) : Stream =
    stream |> writeChunk (serializeIhdrChunkData ihdr)


let readIhdrChunk (stream: Stream) : IhdrData =
    let chunkType, chunkTypeAndDataBytes = readChunk stream

    if chunkType <> ChunkType("IHDR") then
        invalidOp (sprintf "Expected IHDR chunk, but got %A" chunkType)
    else
        deserializeIhdrChunkData chunkTypeAndDataBytes


/// <summary>
/// Serializes the PNG IEND chunk type and data into a byte array.
/// </summary>
/// <returns>
/// The byte array containing the PNG IEND chunk type and data.
/// </returns>
let serializeIendChunkData () : byte[] =
    use stream = new MemoryStream()

    stream |> writeChunkType (ChunkType("IEND")) |> ignore

    stream.ToArray()


let writeIendChunk (stream: Stream) : Stream =
    stream |> writeChunk (serializeIendChunkData ())

/// <summary>
/// Compresses the byte array into the specified output stream using Deflate
/// algorithm.
/// </summary>
/// <param name="data">The byte array to compress.</param>
/// <param name="outputStream">
/// The output stream the compressed data should be written to.</param>
let compress data (outputStream: Stream) : unit =
    // Using the default compression level since I experimented with
    // BEST_COMPRESSION, but it did not improve the compression rate
    // significantly, while it was much slower to execute.
    let deflater = Deflater(Deflater.DEFAULT_COMPRESSION)
    // Using the default deflate strategy since the alternatives did not
    // produce any better results.
    deflater.SetStrategy(DeflateStrategy.Default)
    deflater.SetInput(data)
    deflater.Finish()

    while not deflater.IsFinished do
        let compressionBuffer: byte[] = Array.zeroCreate (100 * 1024 * 4)

        let producedBytesCount = deflater.Deflate(compressionBuffer)
        outputStream.Write(compressionBuffer, 0, producedBytesCount)


/// <summary>
/// Decompresses the compressed byte array into the specified output stream
/// using Inflate algorithm.
/// </summary>
/// <param name="compressedData">The byte array to decompress.</param>
/// <param name="outputStream">
/// The output stream the decompressed data should be written to.</param>
let decompress compressedData (outputStream: Stream) : unit =
    let inflater = Inflater()
    inflater.SetInput(compressedData)

    while not inflater.IsFinished do
        let inflaterBuffer: byte[] = Array.zeroCreate (100 * 1024 * 4)
        let producedBytesCount = inflater.Inflate(inflaterBuffer)
        outputStream.Write(inflaterBuffer, 0, producedBytesCount)


let serializeIdatChunkData
    imageWidth
    imageHeight
    bpp
    (imageData: RawImageData)
    : byte[] =
    let filteredImageData = filterScanlines imageWidth imageHeight bpp imageData
    let dataBeforeCompression = filteredImageData
    use compressionStream = new MemoryStream()

    compressionStream
    |> writeChunkType (ChunkType("IDAT"))
    |> compress dataBeforeCompression

    compressionStream.ToArray()


let deserializeIdatChunkData
    bpp
    imageWidth
    imageHeight
    chunkData
    : RawImageData =
    use chunkDataStream = new MemoryStream()

    chunkDataStream
    // skips the first 4 bytes as there represent the chunk type
    |> decompress (chunkData |> Array.skip 4)

    let decompressedData = chunkDataStream.ToArray()

    let bytesPP = bytesPerPixel bpp
    let filteredScanlineLength = imageWidth * bytesPP + 1
    let scanlinesModulo = decompressedData.Length % filteredScanlineLength

    if scanlinesModulo <> 0 then
        invalidOp "Decompressed IDAT chunk data is invalid."
    else
        let filteredScanlines = decompressedData
        unfilterScanlines imageWidth imageHeight bpp filteredScanlines


let writeIdatChunk
    imageWidth
    imageHeight
    (bpp: int)
    (imageData: RawImageData)
    (stream: Stream)
    : Stream =
    stream
    |> writeChunk (serializeIdatChunkData imageWidth imageHeight bpp imageData)
