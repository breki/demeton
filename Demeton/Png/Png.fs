module Demeton.Png

open Demeton.Binary
open Demeton.PngTypes
open Demeton.PngFilters

open System
open System.IO
open ICSharpCode.SharpZipLib.Zip.Compression


let pngSignature = 
    [| 0x89uy; 0x50uy; 0x4euy; 0x47uy; 0x0duy; 0x0auy; 0x1auy; 0x0auy |]

/// <summary>Writes the 8-byte PNG signature to a stream.</summary>
/// <param name="stream">The stream the signature should be written to.</param>
/// <returns>The same instance of the stream.</returns>
let writeSignature (stream: Stream): Stream =
    Array.ForEach(pngSignature, (fun x -> stream.WriteByte x))
    stream


let readSignature (stream: Stream): Stream =
    let signatureLength = pngSignature.Length

    let signatureRead = 
        [| for i in 0 .. (signatureLength-1) -> readByte stream |]

    if signatureRead = pngSignature then stream
    else invalidOp "Invalid PNG signature"


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


let writeChunk 
    (chunkTypeAndDataBytes: byte[]) 
    (stream: Stream)
    : Stream =
    let chunkDataLength = chunkTypeAndDataBytes.Length - 4

    let chunkCrc = CRC.crc32 chunkTypeAndDataBytes

    stream
    |> writeBigEndianInt32 chunkDataLength
    |> writeBytes chunkTypeAndDataBytes
    |> writeBigEndianUInt32 chunkCrc


let readChunk (stream: Stream): (ChunkType * byte[]) =
    let chunkDataLength = stream |> readBigEndianInt32

    // '4' is for the chunk type
    let chunkTypeAndDataBytes = stream |> readBytes (4 + chunkDataLength)
    let chunkCrc = stream |> readBigEndianUInt32

    let chunkTypeInBytes = chunkTypeAndDataBytes |> Array.take 4

    let chunkType = 
        ChunkType(System.Text.ASCIIEncoding.ASCII.GetString(chunkTypeInBytes))

    let expectedChunkCrc = CRC.crc32 chunkTypeAndDataBytes
    if chunkCrc <> expectedChunkCrc 
        then invalidOp (sprintf "Wrong CRC for PNG chunk type %A." chunkType)
    else 
        (chunkType, chunkTypeAndDataBytes)

let writeIhdrChunk (ihdr: IhdrData) (stream: Stream): Stream =
    stream |> writeChunk (serializeIhdrChunkData ihdr)


let readIhdrChunk (stream: Stream): IhdrData =
    let (chunkType, chunkTypeAndDataBytes) = readChunk stream
    
    if chunkType <> ChunkType("IHDR") then 
        invalidOp (sprintf "Expected IHDR chunk, but got %A" chunkType)
    else
        deserializeIhdrChunkData chunkTypeAndDataBytes


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


let serializeIdatChunkData (scanlines: Scanline[]): byte[] =
    let filteredScanlines = 
        filterScanlines minSumOfAbsoluteValueSelector scanlines
    let dataBeforeCompression = Array.concat filteredScanlines
    use compressionStream = new MemoryStream()

    compressionStream
    |> writeChunkType (ChunkType("IDAT"))
    |> compress dataBeforeCompression
    compressionStream.ToArray()


let deserializeIdatChunkData imageWidth chunkData: Scanline[] =
    use chunkDataStream = new MemoryStream()
    chunkDataStream 
    // skips the first 4 bytes as there represent the chunk type
    |> decompress (chunkData |> Array.skip 4)

    let decompressedData = chunkDataStream.ToArray()
    let filteredScanlines: FilteredScanline[] = 
        decompressedData |> Array.chunkBySize (imageWidth + 1)

    unfilterScanlines filteredScanlines


let writeIdatChunk (scanlines: Scanline[]) (stream: Stream): Stream =
    stream |> writeChunk (serializeIdatChunkData scanlines)


let writeIendChunk (stream: Stream): Stream =
    stream |> writeChunk (serializeIendChunkData())
