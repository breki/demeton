module Tests.Commands_tests.TileShadeCommand.Integration_tests

open Demeton.Commands
open Demeton.Geometry.Common

open Demeton.Shaders
open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
[<Trait("Category", "acceptance")>]
let ``run command`` () =
    let options: TileShadeCommand.Options =
        { TileWidth = 800
          TileHeight = 600
          TileCenter = (7.17, 46)
          MapScale = Some (TileShadeCommand.MapScaleOf 600000)
          Dpi = TileShadeCommand.DefaultDpi
          LambertHillshadingIntensity = 1.
          IgorHillshadingIntensity = 1.
          SlopeShadingIntensity = 1.
          SunAzimuth = IgorHillshader.DefaultSunAzimuth |> degToRad
          SunAltitude = 80. |> degToRad
          WaterBodiesColor = "#49C8FF" |> Png.Rgba8Bit.parseColorHexValue
          LocalCacheDir = TileShadeCommand.DefaultLocalCacheDir
          OutputDir = TileShadeCommand.DefaultOutputDir
          OutputFileName = TileShadeCommand.DefaultOutputFileName }

    let result = TileShadeCommand.run options

    test <@ result |> isOk @>
