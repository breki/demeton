module Tests.Commands_tests.TileShadeCommand.Calculating_geo_area_needed

open Demeton.Commands
open Demeton.Geometry.Common

open Demeton.Shaders
open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
let ``Geo area needed is calculated correctly`` () =
    let options: TileShadeCommand.Options =
        { TileWidth = 800
          TileHeight = 600
          TileCenter = (4, 46)
          PixelSize = None
          MapScale = Some 250000
          Dpi = 245
          IgorHillshadingIntensity = 1.
          SlopeShadingIntensity = 1.
          SunAzimuth = IgorHillshader.DefaultSunAzimuth |> degToRad
          SunAltitude = LambertHillshader.DefaultSunAltitude |> degToRad
          WaterBodiesColor = "#49C8FF" |> Png.Rgba8Bit.parseColorHexValue
          LocalCacheDir = TileShadeCommand.DefaultLocalCacheDir
          OutputDir = TileShadeCommand.DefaultOutputDir
          OutputFileName = TileShadeCommand.DefaultOutputFileName }

    match
        TileShadeCommand.createProjection options
        |> Result.bind (TileShadeCommand.calculateGeoAreaMbr options)
    with
    | Ok geoAreaNeeded ->
        test <@ geoAreaNeeded.MinLon |> isApproxEqualTo 3.866 (Decimals 3) @>
        test <@ geoAreaNeeded.MinLat |> isApproxEqualTo 45.930 (Decimals 3) @>
        test <@ geoAreaNeeded.MaxLon |> isApproxEqualTo 4.134 (Decimals 3) @>
        test <@ geoAreaNeeded.MaxLat |> isApproxEqualTo 46.070 (Decimals 3) @>

    | Error error -> invalidOp error
