module ``Commands tests``.``ShadeCommand``.``Generating shaded tile``

open Raster
open Demeton.Commands
open Demeton.DemTypes
open Demeton.Shaders
open Demeton.Srtm.Types

open Xunit
open Swensen.Unquote
open TestHelp

let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

let mockRasterShader _ _ _ _ = ()

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        FilePrefix = "shading"
        LocalCacheDir = "cache"
        OutputDir = "output"
        SrtmDir = "srtm"
        TileSize = 1000
        RootShadingStep = Pipeline.Common.CustomShading ("some shader")
        ShaderOptions = { Dpi = 300.; MapScale = 5000000. }
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
        (fun _ -> mockRasterShader)
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
            (fun _ -> mockRasterShader)
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
            (fun _ -> mockRasterShader)
            tileRect 
            options 

    test <@ isError shadeTileResult @>

[<Fact>]
let ``Tile generator prepares the tile image data and returns it``() =
    let fetchSomeHeights _ = someHeightsArray

    let mutable imageDataReceived = None
    let shadeRasterReceivesTileRectAndImageData 
        _ tileRectReceived (imageData: RawImageData) _ = 
        imageDataReceived <- Some imageData
        test <@ imageData.Length = 
            tileRect.Width * tileRect.Height * Png.Rgba8Bit.BytesPerPixel @>
        test <@ tileRectReceived = tileRect @>

    let result =
        ShadeCommand.generateShadedRasterTile 
            fetchSomeHeights
            (fun _ -> shadeRasterReceivesTileRectAndImageData)
            tileRect 
            { options with 
                RootShadingStep 
                    = Pipeline.Common.CustomShading "whatever" }

    test <@ result = Ok imageDataReceived @>
