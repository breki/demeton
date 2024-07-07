module Tests.Commands_tests.DemWithWaterBodiesCommand.Integration_tests

open Xunit
open Swensen.Unquote
open TestHelp



[<Fact>]
[<Trait("Category", "acceptance")>]
let ``run command`` () =
    // todo 30: continue augmenting the acceptance test as we progress with the
    // command implementation
    let options: Demeton.Commands.DemWithWaterBodiesCommand.Options =
        { TileId =
            { Lon = { Value = 45 }
              Lat = { Value = 7 } }
          DemResolution = 30
          LocalCacheDir = "cache"
          OutputDir = "output" }

    test <@ Demeton.Commands.DemWithWaterBodiesCommand.run options |> isOk @>
