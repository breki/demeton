module ``Commands tests``.``ShadeCommand``.``Generating shaded tile``

open Demeton
open Demeton.Commands
open Demeton.DemTypes
open Demeton.Srtm.Types
open Png.Types

open Xunit
open Swensen.Unquote
open TestHelp

let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        Dpi = 300.
        FileName = "shading"
        LocalCacheDir = "cache"
        MapScale = 5000000.
        OutputDir = "output"
        SrtmDir = "srtm"
    }

let tileWidth = 500
let tileHeight = 750

let tileRect: Raster.Rect = 
    { MinX = 1119; MinY = 12500; Width = tileWidth; Height = tileHeight }

let someHeightsArray = 
    Ok 
        (Some 
        (HeightsArray(0, 0, 10, 10, 
            HeightsArrayInitializer1D (fun _ -> DemHeightNone))))

let shadeRaster _ _ _ _ = ()

[<Fact>]
let ``Tile generator correctly calculates which SRTM tiles it needs``() =

    let correctSrtmTilesWereRequested (tiles: SrtmTileCoords seq) =
        let tilesArray = tiles |> Seq.toArray

        test <@ tilesArray.Length = 9 @>
        test <@ tilesArray.[0] = { 
            Lon = { Value = 4 }; Lat = { Value = 42 } } @>
        test <@ tilesArray.[8] = { 
            Lon = { Value = 6 }; Lat = { Value = 44 } } @>

        someHeightsArray

    ShadeCommand.generateShadedRasterTile 
        correctSrtmTilesWereRequested
        shadeRaster
        tileRect 
        options 
    |> ignore

    test <@ true @>

[<Fact>]
let ``When heights array fetcher returns None, tile generator does nothing and returns None``() =

    let returnNoneForHeightsArray _ = Ok None

    let shadeTileResult = 
        ShadeCommand.generateShadedRasterTile 
            returnNoneForHeightsArray
            shadeRaster
            tileRect 
            options 

    test <@ isOk shadeTileResult @>
    test <@ shadeTileResult |> isOkValue None @>

[<Fact>]
let ``When heights array fetcher returns an error, tile generator returns an error, too``() =

    let returnErrorInsteadOfHeightsArray _ = Error "something is wrong"

    let shadeTileResult = 
        ShadeCommand.generateShadedRasterTile 
            returnErrorInsteadOfHeightsArray
            shadeRaster
            tileRect 
            options 

    test <@ isError shadeTileResult @>

[<Fact>]
let ``Tile generator prepares the tile image data``() =

    let fetchSomeHeights _ = someHeightsArray

    let shadeRasterReceivesTileRectAndImageData 
        _ tileRectReceived (imageData: RawImageData) _ = 
        test <@ imageData.Length = 
            tileRect.Width * tileRect.Height * Png.Rgba8Bit.BytesPerPixel @>
        test <@ tileRectReceived = tileRect @>

    ShadeCommand.generateShadedRasterTile 
        fetchSomeHeights
        shadeRasterReceivesTileRectAndImageData
        tileRect 
        options 
    |> ignore

    test <@ true @>   
