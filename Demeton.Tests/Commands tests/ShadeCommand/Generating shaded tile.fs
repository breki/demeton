module Tests.``Commands tests``.``ShadeCommand``.``Generating shaded tile``

open Raster
open Demeton.Commands
open Demeton.Shaders
open Demeton.Srtm.Types

open Xunit
open Swensen.Unquote
open TestHelp
open Tests.Srtm.SrtmHelper
open Tests.Shaders

let (area, heights, srtmLevel, mapScale, tileRect) = 
    ShadingSampleGenerator.generateSampleWithParameters
        4.262676 42.90816 16.962471 48.502048 20000000. 72.

let coveragePoints = [(area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat)]

let mockRasterShader _ _ _ _ _ = ()

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        FilePrefix = "shading"
        LocalCacheDir = "cache"
        OutputDir = "output"
        SrtmDir = "srtm"
        TileSize = 1000
        RootShadingStep = Pipeline.Common.CustomShading ("some shader")
        MapScale = mapScale
    }
   
//[<Fact>]
[<Fact (Skip="todo")>]
let ``Tile generator correctly calculates which SRTM tiles it needs``() =

    let correctSrtmTilesWereRequested (tiles: SrtmTileCoords seq) =
        let tilesArray = tiles |> Seq.toArray

        test <@ tilesArray.Length = 9 @>
        test <@ tilesArray.[0] = srtmTileCoords 0 4 40 @>
        test <@ tilesArray.[8] = srtmTileCoords 0 6 42 @>

        heights |> Some |> Ok

    ShadeCommand.generateShadedRasterTile 
        correctSrtmTilesWereRequested
        (fun _ -> mockRasterShader)
        (SrtmLevel.fromInt 0)
        tileRect 
        options 
    |> ignore

    test <@ true @>

[<Fact(Skip="todo")>]
let ``When heights array fetcher returns None, tile generator does nothing and returns None``() =

    let returnNoneForHeightsArray _ = Ok None

    let shadeTileResult = 
        ShadeCommand.generateShadedRasterTile 
            returnNoneForHeightsArray
            (fun _ -> mockRasterShader)
            (SrtmLevel.fromInt 0)
            tileRect 
            options 

    test <@ isOk shadeTileResult @>
    test <@ shadeTileResult |> isOkValue None @>

[<Fact(Skip="todo")>]
let ``When heights array fetcher returns an error, tile generator returns an error, too``() =

    let returnErrorInsteadOfHeightsArray _ = Error "something is wrong"

    let shadeTileResult = 
        ShadeCommand.generateShadedRasterTile 
            returnErrorInsteadOfHeightsArray
            (fun _ -> mockRasterShader)
            (SrtmLevel.fromInt 0)
            tileRect 
            options 

    test <@ isError shadeTileResult @>

[<Fact(Skip="todo")>]
let ``Tile generator prepares the tile image data and returns it``() =
    let fetchSomeHeights _ = heights |> Some |> Ok

    let mutable imageDataReceived = None
    let shadeRasterReceivesTileRectAndImageData 
        _ _ tileRectReceived (imageData: RawImageData) _ = 
        imageDataReceived <- Some imageData
        test <@ imageData.Length = 
            tileRect.Width * tileRect.Height * Png.Rgba8Bit.BytesPerPixel @>
        test <@ tileRectReceived = tileRect @>

    let result =
        ShadeCommand.generateShadedRasterTile 
            fetchSomeHeights
            (fun _ -> shadeRasterReceivesTileRectAndImageData)
            (SrtmLevel.fromInt 0)
            tileRect 
            { options with 
                RootShadingStep 
                    = Pipeline.Common.CustomShading "whatever" }

    test <@ result = Ok imageDataReceived @>
