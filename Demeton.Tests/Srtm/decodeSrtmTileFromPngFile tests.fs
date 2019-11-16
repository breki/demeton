module Tests.Srtm.``decodeSrtmTileFromPngFile tests``

open Demeton.Srtm
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Png
open Png.Types
open Png.File

open Xunit
open Swensen.Unquote
open TestHelp

open System
open System.IO
open System.Reflection

[<Fact>]
[<Trait("Category", "slow")>]
let ``Can decode a valid PNG-encoded SRTM tile``() =
    let tileName = "N46E015"
    
    let prepareSamplePngTile() =
        let pngFilename = sprintf "%s.png" tileName

        let assembly = Assembly.GetExecutingAssembly()
        use resourceStream = 
            assembly.GetManifestResourceStream(
                sprintf "Demeton.Tests.samples.%s" pngFilename)

        match File.Exists(pngFilename) with
        | true -> 
            printfn "Deleting file %s..." pngFilename
        | false -> ()

        printfn "Preparing new instance of %s..." pngFilename
        use pngTempFileStream = File.OpenWrite(pngFilename)
        resourceStream.CopyTo(pngTempFileStream)
        pngTempFileStream.Close()

        pngFilename

    let tileId = parseTileName tileName
    let pngFileName = prepareSamplePngTile()
    let heightsArrayResult =
        decodeSrtmTileFromPngFile
            FileSys.openFileToRead tileId pngFileName

    let (minx, miny) = tileId |> newTileCellMinCoords 3600

    let heightsArray = resultValue heightsArrayResult

    test <@ heightsArray.Width = 3600 @>
    test <@ heightsArray.Height = 3600 @>
    test <@ heightsArray.MinX = minx @>
    test <@ heightsArray.MinY = miny @>

[<Fact>]
[<Trait("Category", "slow")>]
let ``Throws an exception if PNG image size is not of a SRTM tile``() =
    let writeSampleGrayscale16BitImage imageFileName =
        let imageWidth = 500
        let imageHeight = 500

        let ihdr = { 
            Width = imageWidth
            Height = imageHeight 
            BitDepth = PngBitDepth.BitDepth16
            ColorType = PngColorType.Grayscale
            InterlaceMethod = PngInterlaceMethod.NoInterlace
            }

        let rnd = Random(123)

        let initializer = 
            Grayscale16Bit.ImageDataInitializer2D (
                fun _ _ -> (uint16)(rnd.Next(2<<<16-1)))
        let imageData = 
            Grayscale16Bit.createImageData
                imageWidth 
                imageHeight
                initializer

        use stream = File.OpenWrite(imageFileName)

        stream 
        |> savePngToStream ihdr imageData
        |> ignore

    let tileName = "N46E017"
    let tileId = parseTileName tileName
    let pngFileName = sprintf "%s.png" tileName
    
    writeSampleGrayscale16BitImage pngFileName

    test <@ 
            isErrorData 
                "The image size of this PNG does not correspond to the SRTM tile."
                (decodeSrtmTileFromPngFile
                        FileSys.openFileToRead tileId pngFileName)
    @>