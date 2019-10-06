module ``Commands tests``.``ShadeCommand``.``Saving the tile``

open Demeton
open Demeton.Commands
open Png
open Png.Types

open System.IO

open Xunit
open Swensen.Unquote

let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        Dpi = 300.
        FileName = "shading"
        LocalCacheDir = "cache"
        MapScale = 5000000.
        OutputDir = "output"
        SrtmDir = "srtm"
        TileSize = 1000
    }

let tileIndexX = 3
let tileIndexY = 5
let maxTileIndex = 9
let tileRect: Raster.Rect 
    = { MinX = 1023; MinY = 2343; Width = 10; Height = 20 }
let imageData = Rgba8Bit.createImageData 10 20 Rgba8Bit.ImageDataZero

let mutable createdDirectoryName = None
let createDirectory dirName = 
    createdDirectoryName <- Some dirName
    dirName

let mutable ihdrUsed = None
let mutable imageDataUsed = None
let writePngToStream: Png.File.PngStreamWriter =
    fun ihdr imageDataReceived stream ->
        ihdrUsed <- Some ihdr
        imageDataUsed <- Some imageDataReceived
        stream

let mutable pngFileNameUsed = None
let openPngFile fileName =
    pngFileNameUsed <- Some fileName
    new MemoryStream() :> Stream

let saveTile maxTileIndex =
    ShadeCommand.saveShadedRasterTile 
        createDirectory 
        openPngFile
        writePngToStream 
        options 
        maxTileIndex
        (tileIndexX, tileIndexY)
        tileRect 
        imageData

[<Fact>]
let ``The output directory needs to be created``() =
    saveTile maxTileIndex |> ignore

    test <@ createdDirectoryName = Some "output" @>

[<Fact>]
let ``The name of tile PNG file has to be in the required format and is returned by the function``() =
    let returnedFileName = saveTile maxTileIndex

    let expectedFileName = 
        options.OutputDir 
        |> Pth.combine 
            (sprintf "%s-%d-%d.png" options.FileName tileIndexX tileIndexY)
    test <@ pngFileNameUsed = Some expectedFileName @>
    test <@ returnedFileName = expectedFileName @>

[<Theory>]
[<InlineData(30, "03-05")>]
[<InlineData(10, "03-05")>]
[<InlineData(9, "3-5")>]
[<InlineData(1, "3-5")>]
[<InlineData(100, "003-005")>]
[<InlineData(99, "03-05")>]
let ``Zero-pads the tile index numbers in the PNG file name when required``
    (maxTileIndexToUse, expectedTileIndexesString) =

    saveTile maxTileIndexToUse |> ignore

    let expectedFileName = 
        options.OutputDir 
        |> Pth.combine (
            sprintf "%s-%s.png" options.FileName expectedTileIndexesString)
    test <@ pngFileNameUsed = Some expectedFileName @>
    
[<Fact>]
let ``PNG IHDR chunk is correctly filled and the correct image data is provided when saving PNG``() =
    saveTile maxTileIndex |> ignore

    test <@ ihdrUsed |> Option.isSome @>
    let ihdr = Option.get ihdrUsed
    test <@ ihdr = { 
        Width = tileRect.Width
        Height = tileRect.Height
        BitDepth = PngBitDepth.BitDepth8
        ColorType = PngColorType.RgbAlpha
        InterlaceMethod = PngInterlaceMethod.NoInterlace
        }
        @>

    test <@ 
            Option.isSome imageDataUsed 
            && Option.get imageDataUsed = imageData @>
