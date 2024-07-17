module Demeton.Tests.``Reading and writing PNG files``

open Png
open Png.Types
open Png.FileStructure
open Png.Chunks
open Png.File

open FsCheck
open FsCheck.Xunit
open FsUnit
open Xunit
open Swensen.Unquote
open TestHelp

open Raster

open System
open System.IO


let givenA8BitGrayscaleImage rndSeed imageWidth imageHeight : RawImageData =
    let rnd = Random(rndSeed)

    Array.init (imageWidth * imageHeight) (fun _ -> byte (rnd.Next 256))


[<Fact>]
let ``Writes PNG signature into a stream`` () =
    use stream = new MemoryStream()
    writeSignature stream |> ignore

    test
        <@
            stream.ToArray() = [| 0x89uy
                                  0x50uy
                                  0x4euy
                                  0x47uy
                                  0x0duy
                                  0x0auy
                                  0x1auy
                                  0x0auy |]
        @>


[<Fact>]
let ``Writes chunk into a stream`` () =
    let givenSomeChunkData () : byte[] =
        use stream = new MemoryStream()

        stream
        |> writeChunkType (ChunkType("TEST"))
        |> Bnry.writeBigEndianInt32 1212234
        |> ignore

        stream.ToArray()

    use stream = new MemoryStream()
    stream |> writeChunk (givenSomeChunkData ()) |> ignore

    test
        <@
            stream.ToArray() = [| 0x00uy
                                  0x00uy
                                  0x00uy
                                  0x04uy
                                  byte 'T'
                                  byte 'E'
                                  byte 'S'
                                  byte 'T'
                                  0x00uy
                                  0x12uy
                                  0x7Fuy
                                  0x4Auy
                                  196uy
                                  248uy
                                  209uy
                                  112uy |]
        @>


[<Fact>]
let ``Can serialize IHDR chunk into a byte array`` () =
    let chunk =
        { Width = 1200
          Height = 800
          BitDepth = PngBitDepth.BitDepth8
          ColorType = PngColorType.Grayscale
          InterlaceMethod = PngInterlaceMethod.NoInterlace }

    test
        <@
            serializeIhdrChunkData chunk = [| byte 'I'
                                              byte 'H'
                                              byte 'D'
                                              byte 'R'
                                              0x00uy
                                              0x00uy
                                              0x04uy
                                              0xB0uy
                                              0x00uy
                                              0x00uy
                                              0x03uy
                                              0x20uy
                                              0x08uy
                                              0x00uy
                                              0x00uy
                                              0x00uy
                                              0x00uy |]
        @>


[<Property>]
let ``Deserializing serialized IHDR chunk data results in the original IHDR data``
    (ihdrData: IhdrData)
    =
    let deserialized =
        deserializeIhdrChunkData (serializeIhdrChunkData ihdrData)

    deserialized = ihdrData


type TestImageData = ScanlineBitDepthMode * int * int

// todo 5: add sub-byte modes once they are implemented
type TestImageDataGenerator =
    static member TestImageData() =
        let scanlineBitDepthMode =
            Gen.elements [| ByteMode 1; ByteMode 2; ByteMode 3; ByteMode 4 |]

        let imageWidth = Gen.choose (0, 50)
        let imageHeight = Gen.choose (0, 50)

        Gen.zip3 scanlineBitDepthMode imageWidth imageHeight
        |> Gen.map (fun (bpp, w, h) -> TestImageData(bpp, w, h))
        |> Arb.fromGen


type TestImageDataPropertyAttribute() =
    inherit
        PropertyAttribute(
            Arbitrary = [| typeof<TestImageDataGenerator> |],
            QuietOnSuccess = true
        )


[<TestImageDataProperty>]
let ``Deserializing serialized IDAT chunk data results in the original image data``
    (testImageData: TestImageData)
    =

    let scanlineBitDepthMode, imageWidth, imageHeight = testImageData

    let bpp =
        match scanlineBitDepthMode with
        | ByteMode bytes -> bytes * 8
        | SubByteMode bits -> bits

    let rnd = Random()

    match scanlineBitDepthMode with
    | ByteMode bytesPerPixel ->
        let rawImageData =
            Array.init (imageWidth * imageHeight * bytesPerPixel) (fun _ ->
                byte (rnd.Next 256))

        let deserialized =
            deserializeIdatChunkData
                bpp
                imageWidth
                imageHeight
                (serializeIdatChunkData imageWidth imageHeight bpp rawImageData)

        deserialized = rawImageData
    | SubByteMode _ ->
        raise <| InvalidOperationException("Unsupported bit depth mode")


[<Fact>]
let ``Can serialize IEND chunk into a byte array`` () =
    test
        <@
            serializeIendChunkData () = [| byte 'I'
                                           byte 'E'
                                           byte 'N'
                                           byte 'D' |]
        @>


[<Fact>]
let ``Can generate and read a valid 8-bit grayscale PNG`` () =
    let imageWidth = 100
    let imageHeight = 80

    let ihdr =
        { Width = imageWidth
          Height = imageHeight
          BitDepth = PngBitDepth.BitDepth8
          ColorType = PngColorType.Grayscale
          InterlaceMethod = PngInterlaceMethod.NoInterlace }

    let rnd = Random(123)

    let imageData =
        Array.init (imageWidth * imageHeight) (fun _ -> byte (rnd.Next(255)))

    let imageFileName = Path.GetFullPath("test-grayscale-8.png")
    printfn "Saving test image to %s" imageFileName

    use stream = File.OpenWrite(imageFileName)

    stream |> savePngToStream ihdr imageData |> ignore

    stream.Close()

    use readStream = File.OpenRead(imageFileName)

    let _, _ = readStream |> loadPngFromStream

    use bitmap = System.Drawing.Bitmap.FromFile(imageFileName)
    test <@ bitmap.Width = imageWidth @>
    test <@ bitmap.Height = imageHeight @>


[<Fact>]
let ``Can generate and read a valid 16-bit grayscale PNG`` () =
    let imageWidth = 500
    let imageHeight = 500

    let ihdr =
        { Width = imageWidth
          Height = imageHeight
          BitDepth = PngBitDepth.BitDepth16
          ColorType = PngColorType.Grayscale
          InterlaceMethod = PngInterlaceMethod.NoInterlace }

    let rnd = Random(123)

    let initializer =
        Grayscale16Bit.ImageDataInitializer2D(fun _ _ ->
            uint16 (rnd.Next(2 <<< 16 - 1)))

    let imageData =
        Grayscale16Bit.createImageData imageWidth imageHeight initializer

    let imageFileName = Path.GetFullPath("test-grayscale-16.png")
    printfn "Saving test image to %s" imageFileName

    use stream = File.OpenWrite(imageFileName)

    stream |> savePngToStream ihdr imageData |> ignore

    stream.Close()

    use readStream = File.OpenRead(imageFileName)

    let _, _ = readStream |> loadPngFromStream

    // Skipping this check on Linux since the 16-bit grayscale PNGs require a
    // version of libgdiplus that is more recent than the one that is available
    // on Ubuntu 18.04. See https://github.com/mono/libgdiplus/issues/522 for
    // more info.
    if not (isLinux ()) then
        use bitmap = System.Drawing.Bitmap.FromFile(imageFileName)
        test <@ bitmap.Width = imageWidth @>
        test <@ bitmap.Height = imageHeight @>


[<Fact>]
[<Trait("Category", "slow")>]
let ``Can decode 16-bit grayscale image generated from a SRTM tile`` () =
    use pngReadStream = sampleFileStream "N46E015.png"
    let clock = System.Diagnostics.Stopwatch()
    clock.Start()

    printfn "Decoding the PNG..."

    let ihdrRead, imageDataRead = pngReadStream |> loadPngFromStream

    test <@ ihdrRead.Width = 3600 @>
    test <@ ihdrRead.Height = 3600 @>
    test <@ imageDataRead.Length = (3600 * 3600 * 2) @>

    printfn "%d DONE." clock.ElapsedMilliseconds


// todo 1: this test should work
[<Fact(Skip = "in the process of implementing sub-byte scanline mode support")>]
let ``Can generate and read a valid 1-bit grayscale PNG`` () =
    let imageWidth = 500
    let imageHeight = 500

    let ihdr =
        { Width = imageWidth
          Height = imageHeight
          BitDepth = PngBitDepth.BitDepth1
          ColorType = PngColorType.Grayscale
          InterlaceMethod = PngInterlaceMethod.NoInterlace }

    let rnd = Random(123)

    let initializer =
        Grayscale1Bit.ImageDataInitializer2D(fun _ _ -> rnd.Next(2) = 1)

    let imageData =
        Grayscale1Bit.createImageData imageWidth imageHeight initializer

    let imageFileName = Path.GetFullPath("test-grayscale-1.png")
    printfn $"Saving test image to %s{imageFileName}"

    use stream = File.OpenWrite(imageFileName)

    stream |> savePngToStream ihdr imageData |> ignore

    stream.Close()

    use readStream = File.OpenRead(imageFileName)

    let _, _ = readStream |> loadPngFromStream

    // Skipping this check on Linux since the 16-bit grayscale PNGs require a
    // version of libgdiplus that is more recent than the one that is available
    // on Ubuntu 18.04. See https://github.com/mono/libgdiplus/issues/522 for
    // more info.
    if not (isLinux ()) then
        use bitmap = System.Drawing.Bitmap.FromFile(imageFileName)
        test <@ bitmap.Width = imageWidth @>
        test <@ bitmap.Height = imageHeight @>


[<Fact>]
[<Trait("Category", "slow")>]
let ``Can generate and read a valid 8-bit RGBA PNG`` () =
    let imageWidth = 500
    let imageHeight = 500

    let ihdr =
        { Width = imageWidth
          Height = imageHeight
          BitDepth = PngBitDepth.BitDepth8
          ColorType = PngColorType.RgbAlpha
          InterlaceMethod = PngInterlaceMethod.NoInterlace }

    let rnd = Random(123)

    let samplePixelIndex = 3252
    let mutable sampleColor: Rgba8Bit.RgbaColor = 0u

    let initializer =
        Rgba8Bit.ImageDataInitializer1D(fun i ->
            let color =
                Rgba8Bit.rgbaColor
                    (byte (rnd.Next(256)))
                    (byte (rnd.Next(256)))
                    (byte (rnd.Next(256)))
                    (byte (rnd.Next(256)))

            if i = samplePixelIndex then
                sampleColor <- color

            color)

    let imageData = Rgba8Bit.createImageData imageWidth imageHeight initializer

    let samplePixelX = samplePixelIndex % imageWidth
    let samplePixelY = samplePixelIndex / imageWidth

    let samplePixelColorRgba =
        Rgba8Bit.pixelAt imageData imageWidth samplePixelX samplePixelY

    test <@ Rgba8Bit.toHex samplePixelColorRgba = Rgba8Bit.toHex sampleColor @>

    let imageFileName = Path.GetFullPath("test-rgba-8.png")
    printfn "Saving test image to %s" imageFileName

    use stream = File.OpenWrite(imageFileName)

    stream |> savePngToStream ihdr imageData |> ignore

    stream.Close()

    use readStream = File.OpenRead(imageFileName)

    let ihdrRead, imageDataRead = readStream |> loadPngFromStream

    test <@ ihdrRead = ihdr @>
    test <@ imageDataRead = imageData @>

    use bitmap =
        downcast System.Drawing.Bitmap.FromFile(imageFileName)
        : System.Drawing.Bitmap

    test <@ bitmap.Width = imageWidth @>
    test <@ bitmap.Height = imageHeight @>

    test
        <@
            let samplePixelColorHex = Rgba8Bit.toHex samplePixelColorRgba
            let actualPixel = bitmap.GetPixel(samplePixelX, samplePixelY)

            let actualPixelColorRgba =
                Rgba8Bit.rgbaColor
                    actualPixel.R
                    actualPixel.G
                    actualPixel.B
                    actualPixel.A

            let actualPixelColorHex = Rgba8Bit.toHex actualPixelColorRgba
            actualPixelColorHex = samplePixelColorHex
        @>
