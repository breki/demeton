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
          HgtSize = 1201
          LocalCacheDir = "cache" }

    test <@ DemWithWaterBodiesCommand.run options |> isOk @>
