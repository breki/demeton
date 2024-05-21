module Tests.Commands_tests.TileShadeCommand.Creating_map_projection

open Demeton.Commands

open Xunit

[<Fact>]
let ``Projection is created``() =
    let options: TileShadeCommand.Options = {
        TileWidth= 100
        TileHeight = 200
        TileCenter = (10, 20)
        PixelSize= None
        MapScale=  Some 250000
        Dpi=245
        WaterBodiesColor = "#49C8FF" |> Png.Rgba8Bit.parseColorHexValue
    }

    let projection = TileShadeCommand.createProjection options

    match projection with
    | Ok projection -> ()
    | Error error -> invalidOp error
