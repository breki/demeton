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
    : ImageData =
    let rnd = new Random(rndSeed)

    Array.init (imageWidth*imageHeight) (fun _ -> ((byte)(rnd.Next 256)))


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
let ``some test``() =
    let x = gen {
        let! n = Arb.generate<int>
        return n
    }

    printfn "x = %A" x


//type ImageDataGenerator =
//    static member ImageData() =   
//        let bytesPerPixel = Gen.elements [| 1; 2; 3; 4 |]
//        let imageWidth = Gen.choose (0, 250)
//        let imageHeight = Gen.choose (0, 250)
//        let arrayProperties = 
//            Gen.zip3 bytesPerPixel imageWidth imageHeight
//            |> Arb.fromGen

//        //let arraySize = bytesPerPixel * imageWidth * imageHeight

//        let randomByteValue = Arb.generate<byte>
        
//        randomByteValue 
//        |> Gen.a
//        |> Gen.arrayOfLength arraySize
//        |> Arb.fromGen



//[<Property>]
//let ``Deserializing serialized IDAT chunk data results in the original image data``
//    imageWidth imageHeight =

//    //printfn "imageData: %A" imageData
    
//    let bpp = 16

//    let rawImageData = Array.init 
//        (imageWidth * imageHeight * 2)
//        (fun i -> )

//    //printfn "rawImageData: %A" rawImageData
    
//    let deserialized = 
//        deserializeIdatChunkData bpp imageWidth imageHeight
//            (serializeIdatChunkData imageWidth imageHeight bpp rawImageData)    

//    //printfn "deserialized: %A" deserialized

//    deserialized = rawImageData


//[<Fact>]
//let ``Can serialize IEND chunk into a byte array``() =
//    test <@ 
//            serializeIendChunkData() = [| 
//                (byte)'I'; (byte)'E'; (byte)'N'; (byte)'D'
//            |] 
//        @>


[<Fact>]
let ``Can generate a simplest 8-bit grayscale PNG``() =
    let imageWidth = 100
    let imageHeight = 80

    let ihdr = { 
        Width = imageWidth
        Height = imageHeight 
        BitDepth = PngBitDepth.BitDepth8
        ColorType = PngColorType.Grayscale
        InterlaceMethod = PngInterlaceMethod.NoInterlace
        }
    
    let rnd = Random(123)
    let imageData = 
        Array.init (imageWidth*imageHeight) (fun _ -> (byte)(rnd.Next(255)))
    
    use stream = new MemoryStream()
    stream 
    |> savePngToStream ihdr imageData
    |> ignore

    stream.Flush() |> ignore

    stream.Seek(0L, SeekOrigin.Begin) |> ignore
    stream |> readSignature |> ignore
    let readIhdrData = stream |> readIhdrChunk

    let (chunkType, chunkData) = stream |> readChunk
    test <@ chunkType = ChunkType("IDAT") @>

    let imageDataRead = 
        deserializeIdatChunkData 
            ihdr.BitsPerPixel readIhdrData.Width readIhdrData.Height chunkData

    test <@ imageDataRead = imageData @>


[<Fact>]
let ``Can generate and read a valid 8-bit grayscale PNG``() =
    let imageWidth = 100
    let imageHeight = 80

    let ihdr = { 
        Width = imageWidth
        Height = imageHeight 
        BitDepth = PngBitDepth.BitDepth8
        ColorType = PngColorType.Grayscale
        InterlaceMethod = PngInterlaceMethod.NoInterlace
        }

    let rnd = Random(123)
    let imageData = 
        Array.init (imageWidth*imageHeight) (fun _ -> (byte)(rnd.Next(255)))

    let imageFileName = Path.GetFullPath("test-grayscale-8.png")
    printfn "Saving test image to %s" imageFileName

    use stream = File.OpenWrite(imageFileName)

    stream 
    |> savePngToStream ihdr imageData
    |> ignore

    stream.Close() |> ignore

    use readStream = File.OpenRead(imageFileName)

    let (ihdrRead, imageDataRead) = 
        readStream |> loadPngFromStream 

    use bitmap = System.Drawing.Bitmap.FromFile(imageFileName)
    test <@ bitmap.Width = imageWidth @>
    test <@ bitmap.Height = imageHeight @>


[<Fact>]
let ``Generated 16-bit grayscale PNG is recognized by System.Drawing``() =
    let imageWidth = 200
    let imageHeight = 150

    let ihdr = { 
        Width = imageWidth
        Height = imageHeight 
        BitDepth = PngBitDepth.BitDepth16
        ColorType = PngColorType.Grayscale
        InterlaceMethod = PngInterlaceMethod.NoInterlace
        }

    let rnd = Random(123)
    let imageData = 
        grayscale16BitImageData 
            imageWidth 
            imageHeight
            (fun _ _ -> (uint16)(rnd.Next(2<<<16-1)))

    let imageFileName = Path.GetFullPath("test-grayscale-16.png")
    printfn "Saving test image to %s" imageFileName

    use stream = File.OpenWrite(imageFileName)

    stream 
    |> savePngToStream ihdr imageData
    |> ignore

    stream.Close() |> ignore

    use readStream = File.OpenRead(imageFileName)

    let (ihdrRead, imageDataRead) = 
        readStream |> loadPngFromStream 

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

    let (ihdrRead, imageDataRead) = 
        pngReadStream |> loadPngFromStream 

    test <@ ihdrRead.Width = 500 @>
    test <@ ihdrRead.Height = 500 @>
    test <@ imageDataRead.Length = (500*500*2) @>

    printfn "%d DONE." clock.ElapsedMilliseconds
