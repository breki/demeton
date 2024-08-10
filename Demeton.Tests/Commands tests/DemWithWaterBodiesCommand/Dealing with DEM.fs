module Demeton.Tests.Commands_tests.DemWithWaterBodiesCommand.Dealing_with_DEM

open Demeton.Commands
open System

open Demeton.Dem.Types
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Encoding water bodies info into DEM`` () =
    let testWidth = 3
    let testHeight = 3

    let demHeights: DemHeight[] =
        [| 0s; 0s; 100s; 100s; -100s; -100s; -1s; -2s; 1s |]

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

    let encoded =
        dem
        |> DemWithWaterBodiesCommand.encodeWaterBodiesInfoIntoDem waterBodies

    let wb = DemHeightNone

    test
        <@ encoded.Cells = [| 0s; 1s; 100s; 101s; -100s; -99s; -2s; -1s; 0s |] @>


[<Fact>]
let ``Extending heights array for SRTM HGT compatibility`` () =
    let demHeights: DemHeight[] = [| 1s; 2s; 3s; 4s |]

    let dem = HeightsArray(0, 0, 2, 2, HeightsArrayDirectImport demHeights)

    let extendedDem =
        DemWithWaterBodiesCommand.extendHeightsArrayWithAdditionalRowAndColumn
            dem

    let no = DemHeightNone
    test <@ extendedDem.Cells = [| 1s; 2s; no; 3s; 4s; no; no; no; no |] @>
