module Tests.Commands_tests.DemWithWaterBodiesCommand.Integration_tests

open Xunit
open Swensen.Unquote
open TestHelp


// todo 0: implement parameters parsing tests

[<Fact>]
[<Trait("Category", "acceptance")>]
let ``run command`` () =
    // todo 10: start implementing the acceptance test
    let options: Demeton.Commands.DemWithWaterBodiesCommand.Options =
        { TileId =
            { Lon = { Value = 45 }
              Lat = { Value = 7 } }
          DemResolution = 30
          LocalCacheDir = "cache"
          OutputDir = "output" }

    test <@ Demeton.Commands.DemWithWaterBodiesCommand.run options |> isOk @>
