module Demeton.Tests.Commands_tests.DemWithWaterBodiesCommand.Dealing_with_DEM

open Demeton.Commands

open Demeton.Dem.Types
open Xunit
open Swensen.Unquote


[<Fact>]
let ``Extending heights array for SRTM HGT compatibility`` () =
    let demHeights: DemHeight[] = [| 1s; 2s; 3s; 4s |]

    let dem = HeightsArray(0, 0, 2, 2, HeightsArrayDirectImport demHeights)

    let extendedDem =
        DemWithWaterBodiesCommand.extendHeightsArrayWithAdditionalRowAndColumn
            dem

    let no = DemHeightNone
    test <@ extendedDem.Cells = [| 1s; 2s; no; 3s; 4s; no; no; no; no |] @>
