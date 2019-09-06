module Demeton.Tests.``Writing PNG files``

open FsUnit
open Xunit
open Swensen.Unquote
open System
open System.IO

type PngBitDepth = 
    BitDepth1 = 1 
    | BitDepth2 = 2
    | BitDepth4 = 4
    | BitDepth8 = 8
    | BitDepth16 = 16

type PngColorType =
    Grayscale = 0
    | Rgb = 2
    | Indexed = 3
    | GrayscaleAlpha = 4
    | RgbAlpha = 6

type PngCompressionMethod = 
    DeflateInflate = 0

type PngFilterMethod = 
    AdaptiveFiltering = 0

type PngInterlaceMethod =
    NoInterlace = 0
    | Adam7Interlace = 1

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

let writeBigEndian (value: int) (stream: Stream): Stream =
    stream
    |> writeByte ((byte)(value >>> 24))
    |> writeByte ((byte)(value >>> 16))
    |> writeByte ((byte)(value >>> 8))
    |> writeByte ((byte)value)

let writeIhdrChunk (chunk: IhdrChunk) (stream: Stream): Stream =
    stream
    |> writeBigEndian chunk.Width
    |> writeBigEndian chunk.Height
    |> writeByte ((byte)(chunk.BitDepth))
    |> writeByte ((byte)(chunk.ColorType))
    |> writeByte ((byte)(PngCompressionMethod.DeflateInflate))
    |> writeByte ((byte)(PngFilterMethod.AdaptiveFiltering))
    |> writeByte ((byte)(chunk.InterlaceMethod))

[<Fact>]
let ``Writes PNG signature into a stream``() =
    use stream = new MemoryStream()
    writeSignature stream |> ignore
    
    test <@ 
            stream.ToArray() = [| 0x89uy; 0x50uy; 0x4euy; 0x47uy; 0x0duy; 0x0auy; 
                            0x1auy; 0x0auy |] 
    @>

[<Fact>]
let ``Writes IHDR chunk into a stream``() =
    let chunk = 
        { Width = 1200; Height = 800; BitDepth = PngBitDepth.BitDepth8; 
           ColorType = PngColorType.Grayscale; 
           InterlaceMethod = PngInterlaceMethod.NoInterlace }

    use stream = new MemoryStream()
    stream |> writeIhdrChunk chunk |> ignore

    test <@ 
            stream.ToArray() = [| 
                0x00uy; 0x00uy; 0x04uy; 0xB0uy;
                0x00uy; 0x00uy; 0x03uy; 0x20uy;
                0x08uy; 0x00uy; 0x00uy; 0x00uy;
                0x00uy
            |] 
        @>
