module Tests.``Commands tests``.``ShadeCommand``.``Generating shaded tile``

open Raster
open Demeton.Commands
open Demeton.Shaders
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open Demeton.Projections
open Xunit
open Swensen.Unquote
open TestHelp
open Tests.Shaders

let (area, heights, srtmLevel, mapScale, tileRect) = 
    ShadingSampleGenerator.generateSampleWithParameters
        4.262676 42.90816 16.962471 48.502048 5000000. 100.

let coveragePoints = [(area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat)]

let mockRasterShader _ _ _ _ _ _ = ()

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        FilePrefix = "shading"
        LocalCacheDir = "cache"
        OutputDir = "output"
        SrtmDir = "srtm"
        TileSize = 1000
        RootShadingStep = Pipeline.Common.CustomShading ("some shader")
        MapScale = mapScale
        ProjectFunc = Mercator.proj
        InvertFunc = Mercator.inverse
    }
   
[<Fact>]
let ``Tile generator correctly calculates which SRTM tiles it needs``() =

    let correctSrtmTilesWereRequested (tiles: SrtmTileId seq) =
        let tilesArray = tiles |> Seq.toArray

        test <@ tilesArray = [|
            srtmTileId 4 0 -4; srtmTileId 4 1 -4
            srtmTileId 4 0 -3; srtmTileId 4 1 -3
            |] @>

        heights |> Some |> Ok

    ShadeCommand.generateShadedRasterTile 
        correctSrtmTilesWereRequested
        (fun _ -> mockRasterShader)
        srtmLevel
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
            srtmLevel
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
            srtmLevel
            tileRect 
            options 

    test <@ isError shadeTileResult @>

[<Fact>]
let ``Tile generator prepares the tile image data and returns it``() =
    let fetchSomeHeights _ = heights |> Some |> Ok

    let mutable imageDataReceived = None
    let shadeRasterReceivesTileRectAndImageData 
        _ _ tileRectReceived (imageData: RawImageData) _ _ = 
        imageDataReceived <- Some imageData
        test <@ imageData.Length = 
            tileRect.Width * tileRect.Height * Png.Rgba8Bit.BytesPerPixel @>
        test <@ tileRectReceived = tileRect @>

    let result =
        ShadeCommand.generateShadedRasterTile 
            fetchSomeHeights
            (fun _ -> shadeRasterReceivesTileRectAndImageData)
            srtmLevel
            tileRect 
            { options with 
                RootShadingStep 
                    = Pipeline.Common.CustomShading "whatever" }

    test <@ result = Ok imageDataReceived @>
