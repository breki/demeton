module Tests.Dem.``DEM tile tests``

open Demeton.Dem.Types
open Demeton.Geometry.Common
open Demeton.Dem.Funcs

open FsUnit
open Xunit
open Swensen.Unquote

let tileName lon lat =
    let tileId = demTileXYId lon lat
    tileId.FormatLat2Lon3

[<Fact>]
let ``Latitude 0 means north`` () = tileName 10 0 |> should equal "N00E010"

[<Fact>]
let ``Latitude -1 means south`` () =
    tileName 10 -1 |> should equal "S01E010"

[<Fact>]
let ``Longitude 0 means east`` () = tileName 0 10 |> should equal "N10E000"

[<Fact>]
let ``Longitude -1 means west`` () =
    tileName -1 10 |> should equal "N10W001"

[<Fact>]
let ``Can parse north and west tile IDs`` () =
    test
        <@
            parseHgtTileName "N10W001" = { Lon = DemLongitude.fromInt -1
                                           Lat = DemLatitude.fromInt 10 }
        @>

[<Fact>]
let ``Can parse south and east tile IDs`` () =
    test
        <@
            parseHgtTileName "S22E080" = { Lon = DemLongitude.fromInt 80
                                           Lat = DemLatitude.fromInt -22 }
        @>

[<Literal>]
let Multiply_minus_179_with_3600 = -644400

[<Literal>]
let Multiply_80_with_3600 = 288000

[<Theory>]
[<InlineData("N90W179", 1, -179, 90)>]
[<InlineData("N00W179", 1, -179, 0)>]
[<InlineData("N00W179", 3600, Multiply_minus_179_with_3600, 0)>]
[<InlineData("S22E080", 3600, Multiply_80_with_3600, -22 * 3600)>]
let ``Calculates global coordinates for a given tile ID``
    tileName
    tileSize
    expectedMinX
    expectedMinY
    =
    test
        <@
            parseTileName tileName |> tileMinCell tileSize = (expectedMinX,
                                                              expectedMinY)
        @>

[<Theory>]
[<InlineData(0, 0., 90., 1, 0., 90.)>]
[<InlineData(0, 0., 0., 1, 0., 0.)>]
[<InlineData(0, 1., -1., 1, 1., -1.)>]
[<InlineData(0, 0.5, 0.5, 1, 0.5, 0.5)>]
[<InlineData(0, 0., 0., 3600, 0., 0.)>]
[<InlineData(0, 46.557611, 15.6455, 3600, 167607.3996, 56323.8)>]
[<InlineData(1, 0., 90., 1, 0., 45.)>]
[<InlineData(1, 1., -1., 1, 0.5, -0.5)>]
[<InlineData(2, 1., -1., 1, 0.25, -0.25)>]
let ``Calculates fractional global coordinates for given longitude and latitude``
    level
    longitude
    latitude
    tileSize
    expectedGlobalX
    expectedGlobalY
    =

    let level = DemLevel.fromInt level
    let cellsPerDegree = cellsPerDegree tileSize level

    let globalX = longitude |> longitudeToCellX cellsPerDegree
    test <@ globalX = expectedGlobalX @>

    let globalY = latitude |> latitudeToCellY cellsPerDegree
    test <@ globalY = expectedGlobalY @>


open FsCheck
open PropertiesHelp


let ``Tile coordinates properties`` (level, (lon, lat), tileSize) =
    let isApproxEqual b a = abs (a - b) < 0.0001

    let childCellsPerDegree = cellsPerDegree tileSize level

    // get the global coordinates
    let cellX = longitudeToCellX childCellsPerDegree (float lon)
    let cellY = latitudeToCellY childCellsPerDegree (float lat)
    let tileX = cellXToTileX tileSize cellX
    let tileY = cellYToTileY tileSize cellY

    let inversibility1 _ =
        let lon' = cellXToLongitude childCellsPerDegree cellX
        let lat' = cellYToLatitude childCellsPerDegree cellY

        (lon' |> isApproxEqual lon && lat' |> isApproxEqual lat)
        |> Prop.label
            "longitudeToCellX and cellXToLongitude are inverse operations"
        |@ sprintf "%f <> %f or %f <> %f" lon lon' lat lat'

    let inversibility2 _ =
        let cellX' = tileXToCellX tileSize tileX
        let cellY' = tileYToCellY tileSize tileY

        (cellX' |> isApproxEqual cellX && cellY' |> isApproxEqual cellY)
        |> Prop.label "tileXToCellX and cellXToTileX are inverse operations"
        |@ sprintf "%f <> %f or %f <> %f" cellX cellX' cellY cellY'

    let parentCellCoordsAreHalf _ =
        match level with
        | Level0 -> true |> Prop.classify true "level 0"
        | HigherLevel ->
            let childLevel = level.Value
            let parentLevel = childLevel + 1 |> DemLevel.fromInt
            let parentCellsPerDegree = cellsPerDegree tileSize parentLevel
            let parentCellX = longitudeToCellX parentCellsPerDegree (float lon)
            let parentCellY = latitudeToCellY parentCellsPerDegree (float lat)
            let halfChildX = cellX / 2.
            let halfChildY = cellY / 2.

            (halfChildX = parentCellX && halfChildY = parentCellY)
            |> Prop.classify true "level > 0"
            |> Prop.label
                "higher level cell coordinates are 1/2 of child level ones"
            |@ sprintf
                "%g <> %g or %g <> %g"
                halfChildX
                parentCellX
                halfChildY
                parentCellY

    let parentTileCoordsAreHalf _ =
        match level with
        | Level0 -> true |> Prop.classify true "level 0"
        | HigherLevel ->
            let childLevel = level.Value
            let parentLevel = childLevel + 1 |> DemLevel.fromInt
            let parentCellsPerDegree = cellsPerDegree tileSize parentLevel

            let parentTileX =
                longitudeToCellX parentCellsPerDegree (float lon)
                |> cellXToTileX tileSize

            let parentTileY =
                latitudeToCellY parentCellsPerDegree (float lat)
                |> cellYToTileY tileSize

            let halfChildX = tileX / 2.
            let halfChildY = tileY / 2.

            (halfChildX = parentTileX && halfChildY = parentTileY)
            |> Prop.classify true "level > 0"
            |> Prop.label
                "higher level tile coordinates are 1/2 of child level ones"
            |@ sprintf
                "%g <> %g or %g <> %g"
                halfChildX
                parentTileX
                halfChildY
                parentTileY

    let cellsToTilesRelation _ =
        let tileX' = cellXToTileX tileSize (cellX + (float tileSize))
        let tileY' = cellYToTileY tileSize (cellY + (float tileSize))

        ((tileX' |> isApproxEqual (tileX + 1.))
         && (tileY' |> isApproxEqual (tileY + 1.)))
        |> Prop.label "Moving by tileSize cells moves by 1 tile"
        |@ $"%g{tileX + 1.} <> %g{tileX'} or %g{tileY + 1.} <> %g{tileY'}"

    let tileNames _ =
        let tileId =
            demTileId
                level.Value
                (tileX |> floor |> int)
                (tileY |> floor |> int)

        let tileName = toTileName tileId
        let tileId' = parseTileName tileName

        tileId' = tileId
        |> Prop.label "Tile names are properly generated and parsed"

    let conversionToTileCoords _ =
        match level with
        | Level0 ->
            let lonMin = lon |> floor
            let latMin = lat |> floor

            let tileBounds =
                { MinLon = lonMin
                  MinLat = latMin
                  MaxLon = lonMin + 1.
                  MaxLat = latMin + 1. }

            let tiles = tileBounds |> boundsToTiles tileSize level |> Seq.toList

            match tiles with
            | [ tile ] ->
                let actualCoords = tile |> toDemTileCoords

                let expectedCoords =
                    { Lon = DemLongitude.fromInt (lonMin |> int)
                      Lat = DemLatitude.fromInt (latMin |> int) }

                actualCoords = expectedCoords
                |> Prop.label "conversion to SRTM HGT coordinates works"
                |@ sprintf "%A <> %A" actualCoords expectedCoords
            | _ -> invalidOp "bug"

        | HigherLevel -> true |> Prop.classify true "level > 0"

    inversibility1
    .&. inversibility2
    .&. parentCellCoordsAreHalf
    .&. parentTileCoordsAreHalf
    .&. cellsToTilesRelation
    .&. tileNames
    .&. conversionToTileCoords

[<Fact>]
let ``Tile coordinates testing`` () =
    let genTileSize = Gen.choose (1, 100)
    let genLevel = Gen.choose (0, 5) |> Gen.map DemLevel.fromInt
    let genLon = floatInRange -179 180
    let genLat = floatInRange -90 90
    let genPos = Gen.zip genLon genLat

    Gen.zip3 genLevel genPos genTileSize |> Arb.fromGen |> Prop.forAll
    <| ``Tile coordinates properties``
    |> Check.QuickThrowOnFailure
//|> replayPropertyCheck (1800278843,296670464)
