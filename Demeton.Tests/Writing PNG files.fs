module Demeton.Tests.``Writing PNG files``

open Demeton.PngTypes

open FsUnit
open Xunit
open Swensen.Unquote

open System
open System.IO

// https://en.wikipedia.org/wiki/Portable_Network_Graphics
// https://www.w3.org/TR/PNG/#11IHDR
// http://www.libpng.org/pub/png/book/chapter08.html
// https://www.w3.org/TR/REC-png-961001

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
    |> writeByte ((byte)(ihdr.BitDepth))
    |> writeByte ((byte)(ihdr.ColorType))
    |> writeByte ((byte)(PngCompressionMethod.DeflateInflate))
    |> writeByte ((byte)(PngFilterMethod.AdaptiveFiltering))
    |> writeByte ((byte)(ihdr.InterlaceMethod))
    |> ignore

    stream.ToArray()


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
let grayscale8BitScanlines (imageData: Grayscale8BitImageData): byte[] seq =
    seq {
        for y in 0..(Array2D.length2 imageData - 1) do
            yield imageData.[0..(Array2D.length1 imageData - 1),y]
    }


let givenA8BitGrayscaleImage rndSeed imageWidth imageHeight 
    : Grayscale8BitImageData =
    let rnd = new Random(rndSeed)

    Array2D.init imageWidth imageHeight (fun x y -> ((byte)(rnd.Next 256)))


[<Fact>]
let ``Writes PNG signature into a stream``() =
    use stream = new MemoryStream()
    writeSignature stream |> ignore
    
    test <@ 
            stream.ToArray() = [| 0x89uy; 0x50uy; 0x4euy; 0x47uy; 0x0duy; 0x0auy; 
                            0x1auy; 0x0auy |] 
    @>


[<Fact>]
let ``Writes chunk into a stream``() =
    let givenSomeChunkData (): byte[] =
        use stream = new MemoryStream()
        stream 
        |> writeChunkType (new ChunkType("TEST"))
        |> writeBigEndianInt32 1212234 |> ignore
        stream.ToArray();

    use stream = new MemoryStream()
    stream |> writeChunk (givenSomeChunkData) |> ignore

    test <@ 
            stream.ToArray() = [| 
                0x00uy; 0x00uy; 0x00uy; 0x04uy;
                (byte)'T'; (byte)'E'; (byte)'S'; (byte)'T';
                0x00uy; 0x12uy; 0x7Fuy; 0x4Auy;
                196uy; 248uy; 209uy; 112uy
            |] 
        @>


[<Fact>]
let ``Can serialize IHDR chunk into a byte array``() =
    let chunk = 
        { Width = 1200; Height = 800; BitDepth = PngBitDepth.BitDepth8; 
           ColorType = PngColorType.Grayscale; 
           InterlaceMethod = PngInterlaceMethod.NoInterlace }

    test <@ 
            serializeIhdrChunkData chunk = [| 
                (byte)'I'; (byte)'H'; (byte)'D'; (byte)'R';
                0x00uy; 0x00uy; 0x04uy; 0xB0uy;
                0x00uy; 0x00uy; 0x03uy; 0x20uy;
                0x08uy; 0x00uy; 0x00uy; 0x00uy;
                0x00uy
            |] 
        @>


[<Fact>]
let ``Can serialize IEND chunk into a byte array``() =
    test <@ 
            serializeIendChunkData() = [| 
                (byte)'I'; (byte)'E'; (byte)'N'; (byte)'D'
            |] 
        @>


[<Fact>]
let ``Can transform 8-bit grayscale image into a sequence of scanlines``() =
    let getLine line (imageData: Grayscale8BitImageData) =
        imageData.[0..(Array2D.length1 imageData - 1), line]

    let imageWidth = 10
    let imageHeight = 5
    let imageData = givenA8BitGrayscaleImage 123 imageWidth imageHeight

    let scanlines = grayscale8BitScanlines imageData
    test <@ scanlines |> Seq.length = imageHeight @>
    test <@ scanlines |> Seq.exists (fun sc -> sc.Length <> imageWidth) |> not @>
    test <@ scanlines |> Seq.head = getLine 0 imageData @>
    test <@ scanlines |> Seq.skip 1 |> Seq.head = getLine 1 imageData @>



[<Fact>]
let ``Can generate a simplest PNG``() =
    let ihdr = 
        { Width = 1200; Height = 800; BitDepth = PngBitDepth.BitDepth8; 
           ColorType = PngColorType.Grayscale; 
           InterlaceMethod = PngInterlaceMethod.NoInterlace }

    use stream = new MemoryStream()
    stream 
    |> writeSignature 
    |> writeIhdrChunk ihdr
    |> writeIdatChunk
    |> writeIendChunk
