module ``Commands tests``.``ShadeCommand unit tests``

open Demeton
open Demeton.Commands

open Xunit
open Swensen.Unquote

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

[<Fact(Skip="todo")>]
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

