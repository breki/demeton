module ``decodeSrtmTileFromPngFile tests``

open Demeton.Srtm
open Demeton.Srtm.Png
open Png.Types
open Png.PixelFormats
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
    let tileId = "N46E015"
    
    let prepareSamplePngTile() =
        let pngFilename = sprintf "%s.png" tileId

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

    let pngFileName = prepareSamplePngTile()
    let heightsArrayResult =
        decodeSrtmTileFromPngFile
            FileSys.openFileToRead
            pngFileName

    let (minx, miny) = Tile.tileCellMinCoords 3600 (Tile.parseTileId tileId)

    let heightsArray = resultValue heightsArrayResult

    test <@ heightsArray.Width = 3600 @>
    test <@ heightsArray.Height = 3600 @>
    test <@ heightsArray.MinX = minx @>
    test <@ heightsArray.MinY = miny @>

[<Fact>]
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
            Grayscale16BitImageDataInitializer2D (
                fun _ _ -> (uint16)(rnd.Next(2<<<16-1)))
        let imageData = 
            grayscale16BitImageData 
                imageWidth 
                imageHeight
                initializer

        use stream = File.OpenWrite(imageFileName)

        stream 
        |> savePngToStream ihdr imageData
        |> ignore

    let tileId = "N46E017"
    let pngFileName = sprintf "%s.png" tileId
    
    writeSampleGrayscale16BitImage pngFileName

    test <@ 
            isError 
                "The image size of this PNG does not correspond to the SRTM tile."
                (decodeSrtmTileFromPngFile FileSys.openFileToRead pngFileName)
    @>