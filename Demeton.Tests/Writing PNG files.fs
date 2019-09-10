module Demeton.Tests.``Writing PNG files``

open Demeton.PngTypes
open Demeton.Png

open FsCheck
open FsCheck.Xunit
open FsUnit
open Xunit
open Swensen.Unquote

open System
open System.IO


[<Property>]
let ``Reading of big endian int is inverse of writing it`` (value: int) = 
    use stream = new MemoryStream()
    stream |> writeBigEndianInt32 value |> ignore
    stream.Seek (0L, SeekOrigin.Begin) |> ignore
    readBigEndianInt32 stream = value 


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


[<Property>]
let ``Deserializing serialized IHDR chunk data results in the original IHDR data``
    (ihdrData: IhdrData) =
    printf "original: %A\n" ihdrData
    
    let deserialized = deserializeIhdrChunkData (serializeIhdrChunkData ihdrData) 
    
    printf "deserialized: %A\n" deserialized
    deserialized = ihdrData

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
