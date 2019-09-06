module Demeton.Tests.``Writing PNG files``

open CRC

open FsUnit
open Xunit
open Swensen.Unquote
open System
open System.IO

// https://en.wikipedia.org/wiki/Portable_Network_Graphics
// https://www.w3.org/TR/PNG/#11IHDR
// http://www.libpng.org/pub/png/book/chapter08.html
// https://www.w3.org/TR/REC-png-961001

type PngBitDepth = 
    BitDepth1 = 1uy 
    | BitDepth2 = 2uy
    | BitDepth4 = 4uy
    | BitDepth8 = 8uy
    | BitDepth16 = 16uy

type PngColorType =
    Grayscale = 0uy
    | Rgb = 2uy
    | Indexed = 3uy
    | GrayscaleAlpha = 4uy
    | RgbAlpha = 6uy

type PngCompressionMethod = 
    DeflateInflate = 0uy

type PngFilterMethod = 
    AdaptiveFiltering = 0uy

type PngInterlaceMethod =
    NoInterlace = 0uy
    | Adam7Interlace = 1uy

type IhdrChunk = {
        Width: int
        Height: int
        BitDepth: PngBitDepth
        ColorType: PngColorType
        InterlaceMethod: PngInterlaceMethod
    }

let writeSignature (stream: Stream): Stream =
    let pngSignature = [| 0x89uy; 0x50uy; 0x4euy; 0x47uy; 0x0duy; 0x0auy; 
                                0x1auy; 0x0auy |]
    Array.ForEach(pngSignature, (fun x -> stream.WriteByte x))
    stream

let writeByte (value: byte) (stream: Stream): Stream =
    stream.WriteByte(value)
    stream

let writeBytes (bytes: byte[]) (stream: Stream): Stream =
    stream.Write (bytes, 0, bytes.Length)
    stream


let writeBigEndianInt32 (value: int) (stream: Stream): Stream =
    stream
    |> writeByte ((byte)(value >>> 24))
    |> writeByte ((byte)(value >>> 16))
    |> writeByte ((byte)(value >>> 8))
    |> writeByte ((byte)value)

let writeBigEndianUInt32 (value: uint32) (stream: Stream): Stream =
    stream
    |> writeByte ((byte)(value >>> 24))
    |> writeByte ((byte)(value >>> 16))
    |> writeByte ((byte)(value >>> 8))
    |> writeByte ((byte)value)


type ChunkDataWriter = string -> byte[]


let writeChunkType (chunkType: string) (stream: Stream): Stream = 
    for i in 0 .. chunkType.Length - 1 do
        stream |> writeByte ((byte) chunkType.[i]) |> ignore

    stream


let writeIhdrChunkData (chunk: IhdrChunk) (stream: Stream): Stream =
    stream
    |> writeBigEndianInt32 chunk.Width
    |> writeBigEndianInt32 chunk.Height
    |> writeByte ((byte)(chunk.BitDepth))
    |> writeByte ((byte)(chunk.ColorType))
    |> writeByte ((byte)(PngCompressionMethod.DeflateInflate))
    |> writeByte ((byte)(PngFilterMethod.AdaptiveFiltering))
    |> writeByte ((byte)(chunk.InterlaceMethod))


let writeChunk 
    (chunkType: string) 
    (chunkDataWriter: ChunkDataWriter) 
    (stream: Stream)
    : Stream =
    let chunkTypeAndDataBytes = chunkDataWriter chunkType
    let chunkDataLength = chunkTypeAndDataBytes.Length - 4

    let chunkCrc = CRC.crc32 chunkTypeAndDataBytes

    stream
    |> writeBigEndianInt32 chunkDataLength
    |> writeBytes chunkTypeAndDataBytes
    |> writeBigEndianUInt32 chunkCrc


[<Fact>]
let ``Writes PNG signature into a stream``() =
    use stream = new MemoryStream()
    writeSignature stream |> ignore
    
    test <@ 
            stream.ToArray() = [| 0x89uy; 0x50uy; 0x4euy; 0x47uy; 0x0duy; 0x0auy; 
                            0x1auy; 0x0auy |] 
    @>

[<Fact>]
let ``Writes IHDR chunk data into a stream``() =
    let chunk = 
        { Width = 1200; Height = 800; BitDepth = PngBitDepth.BitDepth8; 
           ColorType = PngColorType.Grayscale; 
           InterlaceMethod = PngInterlaceMethod.NoInterlace }

    use stream = new MemoryStream()
    stream |> writeIhdrChunkData chunk |> ignore

    test <@ 
            stream.ToArray() = [| 
                0x00uy; 0x00uy; 0x04uy; 0xB0uy;
                0x00uy; 0x00uy; 0x03uy; 0x20uy;
                0x08uy; 0x00uy; 0x00uy; 0x00uy;
                0x00uy
            |] 
        @>

[<Fact>]
let ``Writes chunk into a stream``() =
    let givenSomeChunkData chunkyType : byte[] =
        use stream = new MemoryStream()
        stream 
        |> writeChunkType chunkyType
        |> writeBigEndianInt32 1212234 |> ignore
        stream.ToArray();

    use stream = new MemoryStream()
    stream |> writeChunk "TEST" givenSomeChunkData |> ignore

    test <@ 
            stream.ToArray() = [| 
                0x00uy; 0x00uy; 0x00uy; 0x04uy;
                (byte)'T'; (byte)'E'; (byte)'S'; (byte)'T';
                0x00uy; 0x12uy; 0x7Fuy; 0x4Auy;
                196uy; 248uy; 209uy; 112uy
            |] 
        @>