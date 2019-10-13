module ``Commands tests``.``ShadeCommand``.``Running the command``

open Demeton
open Demeton.Commands
open Demeton.Shaders.ElevationColoring
open Demeton.Shaders.ShaderTypes

open Xunit
open Swensen.Unquote
open Png

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
        Shader = ElevationColoringShader elevationColorScaleMaperitive
    }

let mutable generatedTiles = []
let mutable savedTiles = []

let tileGenerator 
    (rasterTileCoords: Raster.Rect) _ =
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
let ``Correctly splits the raster into multiple tiles``() =
    initialize()

    ShadeCommand.run options tileGenerator tileSaver |> ignore

    generatedTiles <- generatedTiles |> List.rev

    test <@ generatedTiles.Length = 12 @>
    test <@ generatedTiles.[0] 
        = { MinX = 1119; MinY = 12500; Width = 1000; Height = 1000 } @>
    test <@ generatedTiles.[1] 
        = { MinX = 2119; MinY = 12500; Width = 1000; Height = 1000 } @>
    test <@ generatedTiles.[11] 
        = { MinX = 4119; MinY = 14500; Width = 337; Height = 108 } @>

[<Fact>]
let ``Saves the generated tile images to files``() =
    initialize()

    ShadeCommand.run options tileGenerator tileSaver |> ignore

    savedTiles <- savedTiles |> List.rev

    test <@ savedTiles.Length = 12 @>
    test <@ savedTiles.[0] = "0-0" @>
    test <@ savedTiles.[1] = "1-0" @>
    test <@ savedTiles.[11] = "3-2" @>

[<Fact>]
let ``If generation of a tile fails it records it in the result``() =
    initialize()
    
    let tileGenerator _ _ =
        Error "some error"

    let results = ShadeCommand.run options tileGenerator tileSaver
    test <@ results.Length = 12 @>
    test <@ 
            results 
            |> List.exists (fun x -> x <> Error "some error") 
            |> not
            @>

[<Fact>]
let ``If tile generator returns None, we skip that in the results``() =
    initialize()
    
    let mutable counter = 0
    let tileGeneratorWithNones rasterTile options =
        counter <- counter + 1
        if counter % 2 = 0 then
            Ok None
        else
            tileGenerator rasterTile options

    let results = 
        ShadeCommand.run options tileGeneratorWithNones tileSaver
    test <@ results.Length = 6 @>
