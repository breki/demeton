module Tests.Commands_tests.TileShadeCommand.Integration_tests

open Demeton.Commands

open Xunit

[<Fact>]
[<Trait("Category", "acceptance")>]
let ``run command`` () =
    let options: TileShadeCommand.Options =
        { TileWidth = 800
          TileHeight = 600
          TileCenter = (15.566, 46.5241)
          PixelSize = None
          MapScale = Some 500000
          Dpi = 245
          IgorHillshadingIntensity = 1.
          SlopeShadingIntensity = 1.
          WaterBodiesColor = "#49C8FF" |> Png.Rgba8Bit.parseColorHexValue
          LocalCacheDir = TileShadeCommand.DefaultLocalCacheDir
          OutputDir = TileShadeCommand.DefaultOutputDir }

    TileShadeCommand.run options
