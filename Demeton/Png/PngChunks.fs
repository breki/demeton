module Demeton.PngChunks

open Demeton.Binary
open Demeton.PngTypes
open Demeton.PngFilters
open Demeton.Png

open System.IO
open ICSharpCode.SharpZipLib.Zip.Compression

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
    |> writeChunkType (ChunkType("IHDR"))
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

        if nextByte = expected then stream
        else 
            invalidOp 
                (sprintf "Expected byte is %O but was %O." expected nextByte)

    use stream = new MemoryStream(bytes)

    stream |> readChunkType (ChunkType("IHDR")) |> ignore

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


let writeIhdrChunk (ihdr: IhdrData) (stream: Stream): Stream =
    stream |> writeChunk (serializeIhdrChunkData ihdr)


let readIhdrChunk (stream: Stream): IhdrData =
    let (chunkType, chunkTypeAndDataBytes) = readChunk stream
    
    if chunkType <> ChunkType("IHDR") then 
        invalidOp (sprintf "Expected IHDR chunk, but got %A" chunkType)
    else
        deserializeIhdrChunkData chunkTypeAndDataBytes


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
    |> writeChunkType (ChunkType("IEND"))
    |> ignore

    stream.ToArray()


let writeIendChunk (stream: Stream): Stream =
    stream |> writeChunk (serializeIendChunkData())


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


let serializeIdatChunkData bpp (scanlines: Scanline[]): byte[] =
    let filteredScanlines = 
        filterScanlines minSumOfAbsoluteValueSelector bpp scanlines
    let dataBeforeCompression = Array.concat filteredScanlines
    use compressionStream = new MemoryStream()

    compressionStream
    |> writeChunkType (ChunkType("IDAT"))
    |> compress dataBeforeCompression
    compressionStream.ToArray()


let deserializeIdatChunkData bpp imageWidth chunkData: Scanline[] =
    use chunkDataStream = new MemoryStream()
    chunkDataStream 
    // skips the first 4 bytes as there represent the chunk type
    |> decompress (chunkData |> Array.skip 4)

    let decompressedData = chunkDataStream.ToArray()
    if decompressedData.Length % (imageWidth + 1) <> 0 then
        invalidOp "Decompressed IDAT chunk data is invalid."
    else
        let filteredScanlines: FilteredScanline[] = 
            decompressedData |> Array.chunkBySize (imageWidth + 1)

        unfilterScanlines bpp filteredScanlines


let writeIdatChunk (bpp: int) (scanlines: Scanline[]) (stream: Stream): Stream =
    stream |> writeChunk (serializeIdatChunkData bpp scanlines)
