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

[<Fact>]
let ``Correctly splits the raster into multiple tiles``() =
    let coveragePoints = [(4.262676, 42.90816); (16.962471, 48.502048)]

    let options: ShadeCommand.Options = {
            CoveragePoints = coveragePoints
            Dpi = 300.
            FileName = "shading"
            MapScale = 5000000.
            OutputDir = "output"
        }

    let mutable generatedTiles = []

    let rasterTileGenerator 
        (rasterTileCoords: Raster.Rect) _ _ =
        generatedTiles <- rasterTileCoords :: generatedTiles

    ShadeCommand.run options rasterTileGenerator

    generatedTiles <- generatedTiles |> List.rev

    test <@ generatedTiles.Length = 12 @>
    test <@ generatedTiles.[0] 
        = { MinX = 1119; MinY = 12500; Width = 1000; Height = 1000 } @>
    test <@ generatedTiles.[1] 
        = { MinX = 2119; MinY = 12500; Width = 1000; Height = 1000 } @>
    test <@ generatedTiles.[11] 
        = { MinX = 4119; MinY = 14500; Width = 337; Height = 108 } @>

