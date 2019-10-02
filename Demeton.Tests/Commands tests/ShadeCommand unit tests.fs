module ``Commands tests``.``ShadeCommand unit tests``

open Demeton
open Demeton.Commands

open Xunit
open Swensen.Unquote

[<Fact(Skip="todo once we implement the basic geometry code")>]
let ``Correctly splits the raster into multiple tiles``() =
    let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

    let options: ShadeCommand.Options = {
            CoveragePoints = coveragePoints
            Dpi = 300.
            FileName = "shading"
            MapScale = 1000000.
            OutputDir = "output"
        }

    let mutable generatedTiles = []

    let rasterTileGenerator 
        (rasterTileCoords: Raster.Rect) _ =
        generatedTiles <- rasterTileCoords :: generatedTiles

    ShadeCommand.run options rasterTileGenerator

    test <@ generatedTiles.Length > 0 @>

