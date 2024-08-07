module Tests.Commands_tests.DemWithWaterBodiesCommand.Integration_tests

open Demeton.Commands
open Demeton.Dem.Funcs
open System

open Demeton.Dem.Types
open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
let ``Encoding water bodies info into DEM`` () =
    let testWidth = 3
    let testHeight = 3

    let demHeights: DemHeight[]
        = [| 0s; 0s; 100s; 100s; -100s; -100s; -1s; -2s; 1s |]

    let dem =
        HeightsArray(
            0,
            0,
            testWidth,
            testHeight,
            HeightsArrayDirectImport demHeights
        )

    let waterBodiesHeights: DemHeight[] =
        [| 0s; 1s; 0s; 1s; 0s; 1s; 0s; 1s; 0s |]

    let waterBodies =
        HeightsArray(
            0,
            0,
            testWidth,
            testHeight,
            HeightsArrayDirectImport waterBodiesHeights
        )

    let merged =
        dem |> DemWithWaterBodiesCommand.encodeWaterBodiesInfoIntoDem waterBodies

    let wb = Int16.MinValue

    test <@ merged.Cells
                = [| 0s; 1s; 100s; 101s; -100s; -99s; -2s; -1s; 0s |] @>




[<Fact(Skip = "too slow")>]
[<Trait("Category", "acceptance")>]
let ``run command`` () =
    let options: DemWithWaterBodiesCommand.Options =
        { TileId = demTileXYId 7 45
          DemResolution = 30
          LocalCacheDir = "cache"
          OutputDir = "output" }

    test <@ DemWithWaterBodiesCommand.run options |> isOk @>
