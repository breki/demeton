module ``Commands tests``.``ShadeCommand``.``Running the command``

open Demeton
open Demeton.Commands

open Xunit
open Swensen.Unquote
open Png

let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

let options: ShadeCommand.Options = {
        CoveragePoints = coveragePoints
        Dpi = 300.
        FileName = "shading"
        MapScale = 5000000.
        OutputDir = "output"
    }

let mutable generatedTiles = []
let mutable savedTiles = []

let tileGenerator 
    (rasterTileCoords: Raster.Rect) _ =
    generatedTiles <- rasterTileCoords :: generatedTiles
    Ok (Some (Rgba8Bit.createImageData 10 10 Rgba8Bit.ImageDataZero))

let tileSaver tileX tileY _ _ _ =
    let imageFileName = sprintf "%d-%d" tileX tileY
    savedTiles <- imageFileName :: savedTiles
    imageFileName

[<Fact>]
let ``Correctly splits the raster into multiple tiles``() =
    let tileSaver _ _ _ _ _ = ""

    ShadeCommand.run options tileGenerator tileSaver

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

    ShadeCommand.run options tileGenerator tileSaver

    savedTiles <- savedTiles |> List.rev

    test <@ savedTiles.Length = 12 @>
    test <@ savedTiles.[0] = "0-0" @>
    test <@ savedTiles.[1] = "1-0" @>
    test <@ savedTiles.[11] = "3-2" @>
