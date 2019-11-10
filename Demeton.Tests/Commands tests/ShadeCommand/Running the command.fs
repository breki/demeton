module Tests.``Commands tests``.``ShadeCommand``.``Running the command``

open Demeton.Commands
open Demeton.Shaders

open Xunit
open Swensen.Unquote
open Png
open Tests.Shaders

let (area, heights, srtmLevel, mapScale, tileRect) = 
    ShadingSampleGenerator.generateSampleWithParameters
        10.1 45.5 16.962471 48.502048 5000000. 72.

let coveragePoints = [(area.MinLon, area.MinLat); (area.MaxLon, area.MaxLat)]

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        FilePrefix = "shading"
        LocalCacheDir = "cache"
        OutputDir = "output"
        SrtmDir = "srtm"
        TileSize = 250
        RootShadingStep = Pipeline.Common.ElevationColoring
            { ColorScale = ElevationColoring.colorScaleMaperitive }
        MapScale = mapScale
    }

let mutable generatedTiles = []
let mutable savedTiles = []

let tileGeneratorNeedsSrtmLevel
    : ShadeCommand.ShadedRasterTileGenerator =
    fun providedSrtmLevel _ _ ->
    test <@ srtmLevel = providedSrtmLevel @>
    Ok (Some (Rgba8Bit.createImageData 10 10 Rgba8Bit.ImageDataZero))

let tileGenerator: ShadeCommand.ShadedRasterTileGenerator =
    fun _ rasterTileCoords _ ->
    generatedTiles <- rasterTileCoords :: generatedTiles
    Ok (Some (Rgba8Bit.createImageData 10 10 Rgba8Bit.ImageDataZero))

let tileSaver _ _ (tileX, tileY) _ _ =
    let imageFileName = sprintf "%d-%d" tileX tileY
    savedTiles <- imageFileName :: savedTiles
    imageFileName

let initialize() =
    generatedTiles <- []
    savedTiles <- []

[<Fact>]
let ``Correctly calculates the SRTM level needed``() =
    initialize()

    ShadeCommand.run options tileGeneratorNeedsSrtmLevel tileSaver |> ignore


[<Fact>]
let ``Correctly splits the raster into multiple tiles``() =
    initialize()

    ShadeCommand.run options tileGenerator tileSaver |> ignore

    test <@ generatedTiles = [
        { MinX = 886; MinY = -3256; Width = 184; Height = 28 }
        { MinX = 636; MinY = -3256; Width = 250; Height = 28 }
        { MinX = 886; MinY = -3506; Width = 184; Height = 250 }
        { MinX = 636; MinY = -3506; Width = 250; Height = 250 }
    ] @>

[<Fact>]
let ``Saves the generated tile images to files``() =
    initialize()

    ShadeCommand.run options tileGenerator tileSaver |> ignore

    savedTiles <- savedTiles |> List.rev

    test <@ savedTiles.Length = 4 @>
    test <@ savedTiles.[0] = "0-0" @>
    test <@ savedTiles.[1] = "1-0" @>
    test <@ savedTiles.[2] = "0-1" @>
    test <@ savedTiles.[3] = "1-1" @>

[<Fact>]
let ``If generation of a tile fails it records it in the result``() =
    initialize()
    
    let tileGenerator _ _ _ =
        Error "some error"

    let results = ShadeCommand.run options tileGenerator tileSaver
    test <@ results.Length = 4 @>
    test <@ 
            results 
            |> List.exists (fun x -> x <> Error "some error") 
            |> not
            @>

[<Fact>]
let ``If tile generator returns None, we skip that in the results``() =
    initialize()
    
    let mutable counter = 0
    let tileGeneratorWithNones srtmLevel rasterTile options =
        counter <- counter + 1
        if counter % 2 = 0 then
            Ok None
        else
            tileGenerator srtmLevel rasterTile options

    let results = 
        ShadeCommand.run options tileGeneratorWithNones tileSaver
    test <@ results.Length = 2 @>
