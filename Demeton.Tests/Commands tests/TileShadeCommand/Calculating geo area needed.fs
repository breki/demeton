module Tests.Commands_tests.TileShadeCommand.Calculating_geo_area_needed

open Demeton.Commands

open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
let ``Geo area needed is calculated correctly``() =
    let options: TileShadeCommand.Options = {
        TileWidth = 800
        TileHeight = 600
        TileCenter = (4, 46)
        PixelSize = None
        MapScale = Some 250000
        Dpi = 245
        WaterBodiesColor = "#49C8FF" |> Png.Rgba8Bit.parseColorHexValue
    }

    match
        TileShadeCommand.createProjection options
        |> Result.bind (TileShadeCommand.calculateGeoAreaNeeded options) with
    | Ok geoAreaNeeded ->
        test <@ geoAreaNeeded.MinLon |> isApproxEqualTo 3.866 (Decimals 3)  @>
        test <@ geoAreaNeeded.MinLat |> isApproxEqualTo 45.930 (Decimals 3)  @>
        test <@ geoAreaNeeded.MaxLon |> isApproxEqualTo 4.134 (Decimals 3)  @>
        test <@ geoAreaNeeded.MaxLat |> isApproxEqualTo 46.069 (Decimals 3)  @>

    | Error error -> invalidOp error

