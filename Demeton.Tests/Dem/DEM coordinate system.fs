module Tests.Dem.``DEM coordinate system``

open Demeton.Dem.Funcs
open System

open Demeton.Dem.Types
open Xunit
open FsCheck
open PropertiesHelp

/// <summary>
/// Longitude of 0 degrees corresponds to cell X coordinate of 0.
/// </summary>
let longitudeAndCellXHaveSameCenter ((lon, _), tileSize: int) =
    match lon with
    | 0. ->
        let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
        let cellX = longitudeToCellX cellsPerDegree lon
        cellX = 0
    | _ -> true
    |> Prop.label "longitude and cell X have same center"

/// <summary>
/// Latitude of 0 degrees corresponds to cell Y coordinate of 0.
/// </summary>
let latitudeAndCellYHaveSameCenter ((_, lat), tileSize: int) =
    match lat with
    | 0. ->
        let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
        let cellY = latitudeToCellY cellsPerDegree lat
        cellY = 0
    | _ -> true
    |> Prop.label "latitude and cell Y have same center"

/// <summary>
/// Positive longitude corresponds to positive cell X coordinate.
/// Negative longitude corresponds to negative cell X coordinate.
/// </summary>
let longitudeAndCellXHaveSameSign ((lon, _), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellX = longitudeToCellX cellsPerDegree lon

    Math.Sign cellX = Math.Sign lon
    |> Prop.label "longitude and cell X have same sign"

/// <summary>
/// Positive latitude corresponds to positive cell Y coordinate.
/// Negative latitude corresponds to negative cell Y coordinate.
/// </summary>
let latitudeAndCellYHaveSameSign ((_, lat), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellY = latitudeToCellY cellsPerDegree lat

    Math.Sign cellY = Math.Sign lat
    |> Prop.label "latitude and cell Y have same sign"

/// <summary>
/// The roundtrip calculation from longitude to cell X and back to longitude
/// is correct.
/// </summary>
let longitudeToCellXCalculationRoundtripIsCorrect ((lon, _), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellX = longitudeToCellX cellsPerDegree lon
    let calculatedLon = cellXToLongitude cellsPerDegree cellX

    lon = calculatedLon
    |> Prop.label
        $"longitude ({lon}) to cell X roundtrip calculation is correct ({cellX} -> {calculatedLon})"


/// <summary>
/// The roundtrip calculation from latitude to cell Y and back to latitude
/// is correct.
/// </summary>
let latitudeToCellYCalculationRoundtripIsCorrect ((_, lat), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellY = latitudeToCellY cellsPerDegree lat
    let calculatedLat = cellYToLatitude cellsPerDegree cellY

    lat = calculatedLat
    |> Prop.label
        $"latitude ({lat}) to cell Y roundtrip calculation is correct ({cellY} -> {calculatedLat})"

/// <summary>
/// Tile X and cell X coordinates have the same center.
/// </summary>
let tileXAndCellXHaveSameCenter ((lon, _), tileSize: int) =
    match lon with
    | 0. ->
        let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
        let cellX = longitudeToCellX cellsPerDegree lon
        let tileX = cellXToTileX tileSize cellX
        tileX = 0
    | _ -> true
    |> Prop.label "tile X and cell X coordinates have same center"

/// <summary>
/// Tile Y and cell Y coordinates have the same center.
/// </summary>
let tileYAndCellYHaveSameCenter ((_, lat), tileSize: int) =
    match lat with
    | 0. ->
        let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
        let cellY = latitudeToCellY cellsPerDegree lat
        let tileY = cellYToTileY tileSize cellY
        tileY = 0
    | _ -> true
    |> Prop.label "tile Y and cell Y coordinates have same center"

/// <summary>
/// The roundtrip calculation from tile X to cell X and back is correct.
/// </summary>
let tileXToCellXCalculationRoundtripIsCorrect ((lon, _), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellX = longitudeToCellX cellsPerDegree lon
    let tileX = cellXToTileX tileSize cellX
    let calculatedCellX = tileXToCellX tileSize tileX

    cellX = calculatedCellX
    |> Prop.label
        $"tile X ({tileX}) to cell X roundtrip calculation is correct ({cellX} -> {calculatedCellX})"

/// <summary>
/// The roundtrip calculation from tile Y to cell Y and back is correct.
/// </summary>
let tileYToCellYCalculationRoundtripIsCorrect ((_, lat), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellY = latitudeToCellY cellsPerDegree lat
    let tileY = cellYToTileY tileSize cellY
    let calculatedCellY = tileYToCellY tileSize tileY

    cellY = calculatedCellY
    |> Prop.label
        $"tile Y ({tileY}) to cell Y roundtrip calculation is correct ({cellY} -> {calculatedCellY})"

/// <summary>
/// DemTileCoords correspond to the floor value of the original longitude and latitude.
/// </summary>
let demTileCoordsCorrespondToFloorOfOriginalLonLat ((lon, lat), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellX = longitudeToCellX cellsPerDegree lon
    let cellY = latitudeToCellY cellsPerDegree lat
    let tileX = cellXToTileX tileSize cellX
    let tileY = cellYToTileY tileSize cellY

    let tileId = demTileIdFromFractional 0 tileX tileY
    let demTileCoords = toDemTileCoords tileId

    let label =
        sprintf
            "DemTileCoords (%A) correspond to the floor value of original lon, lat (%f, %f)"
            demTileCoords
            lon
            lat

    (demTileCoords.Lon = DemLongitude.fromInt (lon |> Math.Floor |> int)
     && demTileCoords.Lat = DemLatitude.fromInt (lat |> Math.Floor |> int))
    |> Prop.label label

/// <summary>
/// tileMinCell is southwestern corner of the tile.
/// </summary>
let tileMinCellIsSouthwesternCorner ((lon, lat), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellX = longitudeToCellX cellsPerDegree lon
    let cellY = latitudeToCellY cellsPerDegree lat
    let tileX = cellXToTileX tileSize cellX
    let tileY = cellYToTileY tileSize cellY

    let tileId = demTileIdFromFractional 0 tileX tileY

    let minCellX, minCellY = tileMinCell tileSize tileId

    let label =
        sprintf
            "tileMinCell (%i,%i) is southwestern corner of the tile (%f,%f)"
            minCellX
            minCellY
            cellX
            cellY

    (float minCellX <= cellX && float minCellY <= cellY) |> Prop.label label

/// <summary>
/// Point always fits inside the tile.
/// </summary>
let pointAlwaysFitsInsideTheTile ((lon, lat), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellX = longitudeToCellX cellsPerDegree lon
    let cellY = latitudeToCellY cellsPerDegree lat
    let tileX = cellXToTileX tileSize cellX
    let tileY = cellYToTileY tileSize cellY

    let tileId = demTileIdFromFractional 0 tileX tileY
    let tile11Id = demTileIdFromFractional 0 (tileX + 1.) (tileY + 1.)

    let minCellX, minCellY = tileMinCell tileSize tileId
    let maxCellX, maxCellY = tileMinCell tileSize tile11Id

    let label = sprintf "Point (%f,%f) always fits inside its tile" lon lat

    (float minCellX <= cellX
     && cellX < float maxCellX
     && float minCellY <= cellY
     && cellY < float maxCellY)
    |> Prop.label label


/// <summary>
/// Tile name is as expected.
/// </summary
let tileNameIsAsExpected ((lon, lat), tileSize: int) =
    let cellsPerDegree = cellsPerDegree tileSize (DemLevel.fromInt 0)
    let cellX = longitudeToCellX cellsPerDegree lon
    let cellY = latitudeToCellY cellsPerDegree lat
    let tileX = cellXToTileX tileSize cellX
    let tileY = cellYToTileY tileSize cellY

    let tileId = demTileIdFromFractional 0 tileX tileY
    let tileCoords = toDemTileCoords tileId

    let tileName = tileId.FormatLat2Lon3

    let label =
        sprintf
            "Tile name (%s) is as expected for point (%f,%f)"
            tileName
            lon
            lat

    // todo 0: extend the test to also check the tile X, Y values
    let propertyValid =
        if lat >= 0 then
            tileName.StartsWith "N"
        else
            tileName.StartsWith "S"

    let propertyValid =
        if lon >= 0 then
            propertyValid && tileName[3] = 'E'
        else
            propertyValid && tileName[3] = 'W'

    propertyValid |> Prop.label label

// toHgtTileName
// parseHgtTileName
// parseTileName
// level as property parameter

type DemCoordinateSystemPropertyTest
    (output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    let ``properties of DEM coordinate system`` () =
        let genTileSize = Gen.elements [ 3600; 12000 ]

        let genLon =
            Gen.elements [ 0.; 1.; 1.5; 80.22; -1.; -0.5; 180.; -22.44; -180. ]

        let genLat =
            Gen.elements [ 0.; 1.; 1.5; 80.22; -1.; -0.5; 90.; -22.44; -90. ]

        let genLonLat = Gen.zip genLon genLat
        let gen = Gen.zip genLonLat genTileSize

        let properties x =
            longitudeAndCellXHaveSameCenter x
            .&. latitudeAndCellYHaveSameCenter x
            .&. longitudeAndCellXHaveSameSign x
            .&. latitudeAndCellYHaveSameSign x
            .&. longitudeToCellXCalculationRoundtripIsCorrect x
            .&. latitudeToCellYCalculationRoundtripIsCorrect x
            .&. tileXAndCellXHaveSameCenter x
            .&. tileYAndCellYHaveSameCenter x
            .&. tileXToCellXCalculationRoundtripIsCorrect x
            .&. tileYToCellYCalculationRoundtripIsCorrect x
            .&. demTileCoordsCorrespondToFloorOfOriginalLonLat x
            .&. tileMinCellIsSouthwesternCorner x
            .&. pointAlwaysFitsInsideTheTile x
            .&. tileNameIsAsExpected x

        properties
        // |> Prop.forAll (gen |> Arb.fromGen)
        |> replayPropertyCheck gen output (1272593551, 297342712)


// gen |> Arb.fromGen |> Prop.forAll <| properties
// |> Check.QuickThrowOnFailure
// |> replayPropertyCheck gen output (1272593551, 297342712)
