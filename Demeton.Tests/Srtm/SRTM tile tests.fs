module Tests.Srtm.``SRTM tile tests``

open Demeton.Srtm
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open FsUnit
open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper

[<Fact>]
let ``Latitude 0 means north``() =
    srtmTileCoords 0 10 0
    |> Tile.tileId 
    |> should equal "N00E010"

[<Fact>]
let ``Latitude -1 means south``() =
    srtmTileCoords 0 10 -1
    |> Tile.tileId 
    |> should equal "S01E010"

[<Fact>]
let ``Longitude 0 means east``() =
    srtmTileCoords 0 0 10
    |> Tile.tileId 
    |> should equal "N10E000"

[<Fact>]
let ``Longitude -1 means west``() =
    srtmTileCoords 0 -1 10
    |> Tile.tileId 
    |> should equal "N10W001"

[<Fact>]
let ``Can parse north and west tile IDs``() =
    test <@ Tile.parseTileId 0 "N10W001" = srtmTileCoords 0 -1 10 @>

[<Fact>]
let ``Can parse south and east tile IDs``() =
    test <@ Tile.parseTileId 3 "S22E080" = srtmTileCoords 3 80 -22 @>

[<Literal>]
let Multiply_minus_179_with_3600 = -644400
[<Literal>]
let Multiply_80_with_3600 = 288000

[<Theory>]
[<InlineData("N90W179", 1, -179, -90)>]
[<InlineData("N00W179", 1, -179, 0)>]
[<InlineData("N00W179", 3600, Multiply_minus_179_with_3600, -3599)>]
[<InlineData("S22E080", 3600, Multiply_80_with_3600, 75601)>]
let ``Calculates global coordinates for a given tile ID``
    tileName tileSize expectedMinX expectedMinY =
    test <@ 
            parseTileName tileName
            |> newTileCellMinCoords tileSize = (expectedMinX, expectedMinY)
    @>

[<Theory>]
[<InlineData(0, 0., 90., 1, 0., -90.)>]
[<InlineData(0, 0., 0., 1, 0., 0.)>]
[<InlineData(0, 1., -1., 1, 1., 1.)>]
[<InlineData(0, 0.5, 0.5, 1, 0.5, -0.5)>]
[<InlineData(0, 0., 0., 3600, 0., 0.)>]
[<InlineData(0, 46.557611, 15.6455, 3600, 167607.3996, -56323.8)>]
[<InlineData(1, 0., 90., 1, 0., -45.)>]
[<InlineData(1, 1., -1., 1, 0.5, 0.5)>]
[<InlineData(2, 1., -1., 1, 0.25, 0.25)>]
let ``Calculates fractional global coordinates for given longitude and latitude``
    level longitude latitude tileSize expectedGlobalX expectedGlobalY =

    let srtmLevel = SrtmLevel.fromInt level

    test <@ longitude |> longitudeToCellX tileSize srtmLevel 
                = expectedGlobalX @>
    test <@ latitude |> latitudeToCellY tileSize srtmLevel = expectedGlobalY @>


open FsCheck
open PropertiesHelp


let ``Tile coordinates properties`` (level, (lon, lat), tileSize) =
    let isApproxEqual b a = abs (a-b) < 0.0001

    // get the global coordinates
    let cellX = longitudeToCellX tileSize level (float lon)
    let cellY = latitudeToCellY tileSize level (float lat) 
    let tileX = cellXToTileX tileSize cellX
    let tileY = cellYToTileY tileSize cellY

    let inversibility1 _ = 
        let lon' = cellXToLongitude tileSize level cellX
        let lat' = cellYToLatitude tileSize level cellY

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
        match level.Value with
        | 0 -> true |> Prop.classify true "level 0"

        | childLevel ->
            let parentLevel = childLevel + 1 |> SrtmLevel.fromInt
            let parentCellX = longitudeToCellX tileSize parentLevel (float lon)  
            let parentCellY = latitudeToCellY tileSize parentLevel (float lat)
            let halfChildX = cellX / 2.
            let halfChildY = cellY / 2.
            (halfChildX = parentCellX && halfChildY = parentCellY)
            |> Prop.classify true "level > 0"
            |> Prop.label
                   "higher level cell coordinates are 1/2 of child level ones"
            |@ sprintf
                    "%g <> %g or %g <> %g" 
                    halfChildX parentCellX halfChildY parentCellY
    
    let parentTileCoordsAreHalf _ =
        match level.Value with
        | 0 -> true |> Prop.classify true "level 0"

        | childLevel ->
            let parentLevel = childLevel + 1 |> SrtmLevel.fromInt
            let parentTileX = 
                longitudeToCellX tileSize parentLevel (float lon)  
                |> cellXToTileX tileSize
            let parentTileY = 
                latitudeToCellY tileSize parentLevel (float lat)
                |> cellYToTileY tileSize
            let halfChildX = tileX / 2.
            let halfChildY = tileY / 2.
            (halfChildX = parentTileX && halfChildY = parentTileY)
            |> Prop.classify true "level > 0"
            |> Prop.label
                   "higher level tile coordinates are 1/2 of child level ones"
            |@ sprintf
                    "%g <> %g or %g <> %g" 
                    halfChildX parentTileX halfChildY parentTileY
           
    let cellsToTilesRelation _ =
        let tileX' = cellXToTileX tileSize (cellX + (float tileSize))
        let tileY' = cellYToTileY tileSize (cellY + (float tileSize))

        ((tileX' |> isApproxEqual (tileX + 1.))
            && (tileY' |> isApproxEqual (tileY + 1.)))
        |> Prop.label "Moving by tileSize cells moves by 1 tile"
        |@ sprintf 
            "%g <> %g or %g <> %g" (tileX + 1.) tileX' (tileY + 1.) tileY'

    let tileNames _ =
        let tileId = 
            srtmTileId level.Value
                (tileX |> floor |> int) (tileY |> floor |> int)
        let tileName = toTileName tileId
        let tileId' = parseTileName tileName

        tileId' = tileId
        |> Prop.label "Tile names are properly generated and parsed"

    inversibility1 .&. inversibility2 .&. parentCellCoordsAreHalf
        .&. parentTileCoordsAreHalf .&. cellsToTilesRelation
        .&. tileNames
    
[<Fact>]
let ``Tile coordinates testing``() =
    let genTileSize = Gen.choose(1, 100)
    let genLevel = Gen.choose(0, 5) |> Gen.map SrtmLevel.fromInt
    let genLon = floatInRange -179 180
    let genLat = floatInRange -90 90
    let genPos = Gen.zip genLon genLat
    
    Gen.zip3 genLevel genPos genTileSize
    |> Arb.fromGen
    |> Prop.forAll <| ``Tile coordinates properties``
    |> Check.QuickThrowOnFailure
    //|> replayPropertyCheck (1800278843,296670464)
