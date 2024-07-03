module Tests.Commands_tests.TileShadeCommand.Calculating_geo_area_needed

open Demeton.Commands
open Demeton.Geometry.Common

open Demeton.Shaders
open Raster
open Xunit
open Swensen.Unquote
open TestHelp

let constructOptions mapScale: TileShadeCommand.Options =
        { TileWidth = 800
          TileHeight = 600
          TileCenter = (4, 46)
          MapScale = Some(TileShadeCommand.MapScaleOf mapScale)
          Dpi = 245
          LambertHillshadingIntensity = 1.
          IgorHillshadingIntensity = 1.
          SlopeShadingIntensity = 1.
          SunAzimuth = IgorHillshader.DefaultSunAzimuth |> degToRad
          SunAltitude = LambertHillshader.DefaultSunAltitude |> degToRad
          WaterBodiesColor = "#49C8FF" |> Png.Rgba8Bit.parseColorHexValue
          LocalCacheDir = TileShadeCommand.DefaultLocalCacheDir
          OutputDir = TileShadeCommand.DefaultOutputDir
          OutputFileName = TileShadeCommand.DefaultOutputFileName }

[<Fact>]
let ``Geo area needed is calculated correctly (low map scale)`` () =
    let options = constructOptions 250000

    let bitmapRect: Rect =
        { MinX = -options.TileWidth / 2
          MinY = -options.TileHeight / 2
          Width = options.TileWidth
          Height = options.TileHeight }

    match
        TileShadeCommand.createProjection options
        |> Result.bind (TileShadeCommand.calculateGeoAreaMbr bitmapRect)
    with
    | Ok geoAreaNeeded ->
        test <@ geoAreaNeeded.MinLon |> isApproxEqualTo 3.866 (Decimals 3) @>
        test <@ geoAreaNeeded.MinLat |> isApproxEqualTo 45.930 (Decimals 3) @>
        test <@ geoAreaNeeded.MaxLon |> isApproxEqualTo 4.134 (Decimals 3) @>
        test <@ geoAreaNeeded.MaxLat |> isApproxEqualTo 46.070 (Decimals 3) @>

    | Error error -> invalidOp error

[<Fact>]
let ``Geo area needed is calculated correctly (medium map scale)`` () =
    let options = constructOptions 2500000

    let bitmapRect: Rect =
        { MinX = -options.TileWidth / 2
          MinY = -options.TileHeight / 2
          Width = options.TileWidth
          Height = options.TileHeight }

    match
        TileShadeCommand.createProjection options
        |> Result.bind (TileShadeCommand.calculateGeoAreaMbr bitmapRect)
    with
    | Ok geoAreaNeeded ->
        test <@ geoAreaNeeded.MinLon |> isApproxEqualTo 2.645 (Decimals 3) @>
        test <@ geoAreaNeeded.MinLat |> isApproxEqualTo 45.293 (Decimals 3) @>
        test <@ geoAreaNeeded.MaxLon |> isApproxEqualTo 5.355 (Decimals 3) @>
        test <@ geoAreaNeeded.MaxLat |> isApproxEqualTo 46.699 (Decimals 3) @>

    | Error error -> invalidOp error
