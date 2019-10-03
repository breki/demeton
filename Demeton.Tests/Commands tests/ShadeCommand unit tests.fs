module ``Commands tests``.``ShadeCommand unit tests``

open Demeton
open Demeton.Commands
open Demeton.DemTypes
open Demeton.Srtm.Types

open Xunit
open Swensen.Unquote
open TestHelp
open Png

[<Fact>]
let ``Correctly splits into intervals when all intervals will have the same size``() =
    test <@ ShadeCommand.splitIntoIntervals 10 100 30 |> Seq.toList
                = [ (10, 40); (40, 70); (70, 100) ]
        @>

[<Fact>]
let ``Correctly splits into intervals when the last interval will be smaller``() =
    test <@ ShadeCommand.splitIntoIntervals 10 90 30 |> Seq.toList
                = [ (10, 40); (40, 70); (70, 90) ]
        @>

[<Fact>]
let ``Correctly splits into a single interval``() =
    test <@ ShadeCommand.splitIntoIntervals 10 90 100 |> Seq.toList
                = [ (10, 90) ]
        @>

let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        Dpi = 300.
        FileName = "shading"
        MapScale = 5000000.
        OutputDir = "output"
    }

[<Fact>]
let ``Correctly splits the raster into multiple tiles``() =
    let mutable generatedTiles = []

    let tileGenerator 
        (rasterTileCoords: Raster.Rect) _ =
        generatedTiles <- rasterTileCoords :: generatedTiles

    ShadeCommand.run options tileGenerator

    generatedTiles <- generatedTiles |> List.rev

    test <@ generatedTiles.Length = 12 @>
    test <@ generatedTiles.[0] 
        = { MinX = 1119; MinY = 12500; Width = 1000; Height = 1000 } @>
    test <@ generatedTiles.[1] 
        = { MinX = 2119; MinY = 12500; Width = 1000; Height = 1000 } @>
    test <@ generatedTiles.[11] 
        = { MinX = 4119; MinY = 14500; Width = 337; Height = 108 } @>

[<Fact>]
let ``Tile generator correctly calculates which SRTM tiles it needs``() =

    let tileRect: Raster.Rect = 
        { MinX = 1119; MinY = 12500; Width = 1000; Height = 1000 }

    let heightsArrayFetcher (tiles: SrtmTileCoords seq) =
        let tilesArray = tiles |> Seq.toArray

        test <@ tilesArray.Length = 20 @>
        test <@ tilesArray.[0] = { 
            Lon = { Value = 4 }; Lat = { Value = 42 } } @>
        test <@ tilesArray.[19] = { 
            Lon = { Value = 8 }; Lat = { Value = 45 } } @>

        Ok 
            (Some 
                (HeightsArray(0, 0, 10, 10, 
                    HeightsArrayInitializer1D (fun _ -> DemHeightNone))))

    ShadeCommand.generateShadedRasterTile 
        tileRect 
        options 
        heightsArrayFetcher
        (fun _ _ _ _ -> ()) |> ignore

    test <@ true @>

[<Fact>]
let ``When heights array fetcher returns None, tile generator does nothing and returns None``() =
    let tileWidth = 500
    let tileHeight = 750

    let tileRect: Raster.Rect = 
        { MinX = 1119; MinY = 12500; Width = tileWidth; Height = tileHeight }

    let heightsArrayFetcher _ =
        Ok None

    let shadeRaster _ _ _ _ = ()

    let shadeTileResult = 
        ShadeCommand.generateShadedRasterTile 
            tileRect 
            options 
            heightsArrayFetcher
            shadeRaster

    test <@ isOk shadeTileResult @>
    test <@ shadeTileResult |> isOkValue None @>

[<Fact>]
let ``When heights array fetcher returns an error, tile generator returns an error, too``() =
    let tileWidth = 500
    let tileHeight = 750

    let tileRect: Raster.Rect = 
        { MinX = 1119; MinY = 12500; Width = tileWidth; Height = tileHeight }

    let heightsArrayFetcher _ =
        Error "something is wrong"

    let shadeRaster _ _ _ _ = ()

    let shadeTileResult = 
        ShadeCommand.generateShadedRasterTile 
            tileRect 
            options 
            heightsArrayFetcher
            shadeRaster

    test <@ isError shadeTileResult @>

[<Fact>]
let ``Tile generator prepares the tile image data``() =
    let tileWidth = 500
    let tileHeight = 750

    let tileRect: Raster.Rect = 
        { MinX = 1119; MinY = 12500; Width = tileWidth; Height = tileHeight }

    let heightsArrayFetcher _ =
        Ok 
            (Some 
                (HeightsArray(0, 0, 10, 10, 
                    HeightsArrayInitializer1D (fun _ -> DemHeightNone))))

    let shadeRaster _ tileRectReceived _ _ = 
        test <@ tileRectReceived = tileRect @>

    ShadeCommand.generateShadedRasterTile 
        tileRect 
        options 
        heightsArrayFetcher
        shadeRaster
    |> ignore

    test <@ true @>
    
[<Fact>]
let ``Raster shader colors all of the image``() =
    let imageWidth = 10
    let imageHeight = 10

    let tileRect: Raster.Rect = 
        { MinX = 1119; MinY = 12500; Width = imageWidth; Height = imageHeight }

    let imageData =
        Rgba8Bit.createImageData imageWidth imageHeight Rgba8Bit.ImageDataZero

    let heightsArray = 
        HeightsArray(
            659736, 478459, 1000, 1000, HeightsArrayInitializer1D (
                fun _ -> DemHeight 1000))

    ShadeCommand.shadeRaster heightsArray tileRect imageData options

    let mutable anyNonColoredPixel = false
    for y in 0 .. (imageHeight-1) do
        for x in 0 .. (imageWidth-1) do
            let pixel = Rgba8Bit.pixelAt imageData imageWidth x y
            match Rgba8Bit.r pixel with
            | 0uy -> anyNonColoredPixel <- true
            | _ -> ()

    test <@ not anyNonColoredPixel @>
