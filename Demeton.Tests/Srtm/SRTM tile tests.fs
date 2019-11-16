module Tests.Srtm.``SRTM tile tests``

open Demeton.Srtm
open Demeton.Srtm.Types
open Demeton.DemTypes

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
let Multiply_90_with_3600_minus_3599 = 320401
[<Literal>]
let Multiply_90_plus_22_with_3600_minus_3599 = 399601

[<Theory>]
[<InlineData("N90W179", 1, 0, 0)>]
[<InlineData("N00W179", 1, 0, 90)>]
[<InlineData("N00W179", 3600, 0, Multiply_90_with_3600_minus_3599)>]
[<InlineData("S22E080", 3600, 932400, Multiply_90_plus_22_with_3600_minus_3599)>]
let ``Calculates global coordinates for a given tile ID``
    tileId tileSize expectedMinX expectedMinY =
    test <@ 
            Tile.parseTileId 0 tileId
            |> Tile.tileCellMinCoords tileSize = (expectedMinX, expectedMinY)
    @>

[<Theory>]
[<InlineData(0, 0., 90., 1, 179., 0.)>]
[<InlineData(0, 0., 0., 1, 179., 90.)>]
[<InlineData(0, 1., -1., 1, 180., 91.)>]
[<InlineData(0, 0.5, 0.5, 1, 179.5, 89.5)>]
[<InlineData(0, 0., 0., 3600, 644400., 324000.)>]
[<InlineData(0, 46.557611, 15.6455, 3600, 812007.3996, 267676.2)>]
[<InlineData(1, 0., 90., 1, 89.5, 0.)>]
[<InlineData(1, 1., -1., 1, 90., 45.5)>]
[<InlineData(2, 1., -1., 1, 45., 22.75)>]
let ``Calculates fractional global coordinates for given longitude and latitude``
    level longitude latitude tileSize expectedGlobalX expectedGlobalY =

    let srtmLevel = SrtmLevel.fromInt level

    test <@ Tile.longitudeToGlobalX longitude srtmLevel tileSize 
                = expectedGlobalX @>
    test <@ Tile.latitudeToGlobalY latitude srtmLevel tileSize 
                = expectedGlobalY @>

open FsCheck
open PropertiesHelp

let levelFactor (level: SrtmLevel) =
    1 <<< level.Value

let levelFactorFloat (level: SrtmLevel) =
    1 <<< level.Value |> float

let newLongitudeToGlobalX (tileSize: int) (level: SrtmLevel) (lon: float) =
    let tileColumn = lon / levelFactorFloat level
    tileColumn * (tileSize |> float)

let newLatitudeToGlobalY (tileSize: int) (level: SrtmLevel) (lat: float) =
    let tileRow = -lat / levelFactorFloat level
    tileRow * (tileSize |> float)

let newGlobalXToLongitude (tileSize: int) (level: SrtmLevel) (x: float) =
    let levelFactor = levelFactorFloat level
    x / (tileSize |> float) * levelFactor 

let newGlobalYToLatitude (tileSize: int) (level: SrtmLevel) (y: float) =
    let levelFactor = levelFactorFloat level
    -y / (tileSize |> float) * levelFactor 

let newTileCellMinCoords (tileSize: int) (tileCoords: SrtmTileCoords)
    : GlobalCellCoords =
    let x =
        newLongitudeToGlobalX tileSize tileCoords.Level
            (tileCoords.Lon.Value |> float)
    let y =
        newLatitudeToGlobalY tileSize tileCoords.Level
            (tileCoords.Lat.Value |> float)
    
    (x |> System.Math.Round |> int,
     (y |> System.Math.Round |> int) - (tileSize - 1))

let findTileFromGlobalCoordinates tileSize (level: SrtmLevel) x y =
    let lon = newGlobalXToLongitude tileSize level x
    let lat = newGlobalYToLatitude tileSize level y
    
    let levelFactor = levelFactor level
    let levelFactorFloat = levelFactorFloat level
    let tileLon = (lon |> int) / levelFactor * levelFactor
    let tileLat = ((lat / levelFactorFloat) |> floor |> int) * levelFactor
    
    { Level = level;
      Lon = SrtmLongitude.fromInt tileLon;
      Lat = SrtmLatitude.fromInt tileLat }

let srtmTileCoordsFromFloat level lon lat = 
    { Level = level;
      Lon = SrtmLongitude.fromInt (lon |> int)
      Lat = SrtmLatitude.fromInt (lat |> int) }

let ``Tile coordinates properties``(level, (lon, lat), tileSize) =
    // get the global coordinates
    let x = newLongitudeToGlobalX tileSize level (float lon)
    let y = newLatitudeToGlobalY tileSize level (float lat) 
    
//    let inversibility =
//        (newGlobalXToLongitude x = float lon)
    
    let prop2 _ =
        match level.Value with
        | 0 -> true |> Prop.classify true "level 0"

        | childLevel ->
            let parentLevel = childLevel + 1 |> SrtmLevel.fromInt
            let parentX =
                newLongitudeToGlobalX tileSize parentLevel (float lon)  
            let parentY =
                newLatitudeToGlobalY tileSize parentLevel (float lat)
            let halfChildX = x / 2.
            let halfChildY = y / 2.
            (halfChildX = parentX && halfChildY = parentY)
            |> Prop.classify true "level > 0"
            |> Prop.label
                   "higher level global coordinates are 1/2 of child level ones"
            |@ sprintf
                    "%g <> %g or %g <> %g" halfChildX parentX halfChildY parentY
    
    let containingTile = 
        findTileFromGlobalCoordinates tileSize level x y
    let tileLon = containingTile.Lon.Value
    let tileLat = containingTile.Lat.Value
    
    let propTileCoordsAreEven _ =
        match level.Value with
        | 0 -> true |> Prop.classify true "level 0"
        | _ ->
            (tileLon % 2 = 0 && tileLat % 2 = 0)
            |> Prop.label "tile lon/lat values are even (level > 0)"
    
    let prop3 _ =
        (tileLon >= -179 && tileLon <= 180 && tileLat >= -90 && tileLat <= 90)
        |> Prop.label "tile lon/lat are within valid range"
        
    let tileCoords = srtmTileCoordsFromFloat level tileLon tileLat
    let (tileMinX, tileMinY) = newTileCellMinCoords tileSize tileCoords

    let tileContainingMinCoords =
        findTileFromGlobalCoordinates
            tileSize level (float tileMinX) (float tileMinY)

    let prop4 _ =
        (tileContainingMinCoords = tileCoords)
        |> Prop.label "tile min global coordinates are calculated correctly"
        |@ sprintf "%A <> %A" tileContainingMinCoords tileCoords
                
                
    propTileCoordsAreEven .&. prop2 .&. prop3 .&. prop4
    
[<Fact(Skip="todo: implement a new tile coordinate sytem")>]
let ``Tile coordinates testing``() =
    let genTileSize = Gen.choose(1, 100)
    let genLevel = Gen.choose(0, 5) |> Gen.map SrtmLevel.fromInt
    let genLon = Gen.choose(-179, 180)
    let genLat = Gen.choose(-90, 90)
    let genPos = Gen.zip genLon genLat
    
    Gen.zip3 genLevel genPos genTileSize
    |> Arb.fromGen
    |> Prop.forAll <| ``Tile coordinates properties``
//    |> Check.QuickThrowOnFailure
    |> replayPropertyCheck (1800278843,296670464)
