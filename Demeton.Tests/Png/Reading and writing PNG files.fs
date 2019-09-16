module Demeton.Tests.``Reading and writing PNG files``

open Demeton.Binary
open Demeton.PngTypes
open Demeton.Png
open Demeton.PngChunks
open Demeton.PngPixelFormats

open FsCheck
open FsCheck.Xunit
open FsUnit
open Xunit
open Swensen.Unquote

open System
open System.IO
open System.Reflection


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
    stream |> writeChunk (givenSomeChunkData()) |> ignore

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
    let deserialized = deserializeIhdrChunkData (serializeIhdrChunkData ihdrData)    
    deserialized = ihdrData

[<Property>]
let ``Deserializing serialized IDAT chunk data results in the original image data``
    (imageData: Grayscale16BitImageData) =
    
    let bpp = 16

    let imageWidth = Array2D.length1 imageData
    let imageHeight = Array2D.length2 imageData
    let scanlines = grayscale16BitScanlines imageData
    let imageDataBytes = scanlines |> Array.concat

    let deserialized = 
        deserializeIdatChunkData bpp imageWidth imageHeight
            (serializeIdatChunkData bpp scanlines)    

    deserialized = imageDataBytes


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
let ``Can generate a simplest 8-bit grayscale PNG``() =
    let imageWidth = 100
    let imageHeight = 80
    let bpp = 8

    let rnd = Random(123)
    let imageData = 
        Array2D.init imageWidth imageHeight 
            (fun _ _ -> (byte)(rnd.Next(255)))
    
    use stream = new MemoryStream()
    stream 
    |> saveGrayscale8BitToStream imageData
    |> ignore

    stream.Flush() |> ignore

    stream.Seek(0L, SeekOrigin.Begin) |> ignore
    stream |> readSignature |> ignore
    let readIhdrData = stream |> readIhdrChunk

    let (chunkType, chunkData) = stream |> readChunk
    test <@ chunkType = ChunkType("IDAT") @>

    let scanlinesRead = 
        deserializeIdatChunkData 
            bpp readIhdrData.Width readIhdrData.Height chunkData
    let imageDataRead = 
        scanlinesToGrayscale8Bit 
            readIhdrData.Width readIhdrData.Height scanlinesRead

    test <@ imageDataRead = imageData @>


[<Fact>]
let ``Can generate and read a valid 8-bit grayscale PNG``() =
    let imageWidth = 100
    let imageHeight = 80

    let rnd = Random(123)
    let imageData = 
        Array2D.init imageWidth imageHeight (fun _ _ -> (byte)(rnd.Next(255)))

    let imageFileName = Path.GetFullPath("test-grayscale-8.png")
    printfn "Saving test image to %s" imageFileName

    use stream = File.OpenWrite(imageFileName)

    stream 
    |> saveGrayscale8BitToStream imageData
    |> ignore

    stream.Close() |> ignore

    use readStream = File.OpenRead(imageFileName)

    readStream
    |> loadPngFromStream (fun _ -> ()) (fun _ -> ())
    |> ignore

    use bitmap = System.Drawing.Bitmap.FromFile(imageFileName)
    test <@ bitmap.Width = imageWidth @>
    test <@ bitmap.Height = imageHeight @>


[<Fact>]
let ``Generated 16-bit grayscale PNG is recognized by System.Drawing``() =
    let imageWidth = 200
    let imageHeight = 150

    let rnd = Random(123)
    let imageData = 
        Array2D.init 
            imageWidth imageHeight (fun _ _ -> (uint16)(rnd.Next(2<<<16-1)))

    let imageFileName = Path.GetFullPath("test-grayscale-16.png")
    printfn "Saving test image to %s" imageFileName

    use stream = File.OpenWrite(imageFileName)

    stream 
    |> saveGrayscale16BitToStream imageData
    |> ignore

    stream.Close() |> ignore

    use readStream = File.OpenRead(imageFileName)

    readStream
    |> loadPngFromStream (fun _ -> ()) (fun _ -> ())
    |> ignore

    use bitmap = System.Drawing.Bitmap.FromFile(imageFileName)
    test <@ bitmap.Width = imageWidth @>
    test <@ bitmap.Height = imageHeight @>


[<Fact>]
[<Trait("Category", "slow")>]
let ``Can decode 16-bit grayscale image generated from a SRTM tile``() =
    let assembly = Assembly.GetExecutingAssembly()
    use pngReadStream = assembly.GetManifestResourceStream
                                ("Demeton.Tests.samples.test-grayscale-16.png")
    
    let clock = new System.Diagnostics.Stopwatch()
    clock.Start()

    printfn "Decoding the PNG..."

    let readSrtmImageData (imageData: Grayscale16BitImageData) = 
        test <@ Array2D.length1 imageData = 500 @>
        test <@ Array2D.length2 imageData = 500 @>

    pngReadStream
    |> loadPngFromStream (fun _ -> ()) readSrtmImageData
    |> ignore

    printfn "%d DONE." clock.ElapsedMilliseconds
