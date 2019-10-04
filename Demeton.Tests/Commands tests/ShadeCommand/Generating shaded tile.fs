module ``Commands tests``.``ShadeCommand``.``Generating shaded tile``

open Demeton
open Demeton.Commands
open Demeton.DemTypes
open Demeton.Srtm.Types

open Xunit
open Swensen.Unquote
open TestHelp

let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        Dpi = 300.
        FileName = "shading"
        MapScale = 5000000.
        OutputDir = "output"
    }

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
