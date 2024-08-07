module Tests.Commands_tests.DemWithWaterBodiesCommand.Integration_tests

open Demeton.Commands
open Demeton.Dem.Funcs

open Xunit
open Swensen.Unquote
open TestHelp


[<Fact(Skip = "too slow")>]
[<Trait("Category", "acceptance")>]
let ``run command`` () =
    let options: DemWithWaterBodiesCommand.Options =
        { TileId = demTileXYId 7 45
          DemResolution = 30
          LocalCacheDir = "cache"
          OutputDir = "output" }

    test <@ DemWithWaterBodiesCommand.run options |> isOk @>
