module Demeton.Dem.Funcs

open System
open Demeton.Dem.Types
open Demeton.Geometry.Common


let inline private levelFactorFloat (level: DemLevel) =
    1 <<< level.Value |> float

let inline tileXToCellX (tileSize: int) (tileX: DemTileFractionalX) =
    tileX * (tileSize |> float)

let inline tileYToCellY (tileSize: int) (tileY: DemTileFractionalY) =
    tileY * (tileSize |> float)

/// <summary>
/// Converts DEM cell X coordinate to a fractional tile X coordinate.
/// </summary>
let inline cellXToTileX (tileSize: int) (cellX: DemCellFractionalX) =
    cellX / (tileSize |> float)

/// <summary>
/// Converts DEM cell Y coordinate to a fractional tile Y coordinate.
/// </summary>
let inline cellYToTileY (tileSize: int) (cellY: DemCellFractionalY) =
    cellY / (tileSize |> float)

/// <summary>
/// Calculates the minimum cell coordinates for a given DEM tile.
/// </summary>
/// <param name="tileSize">The size of the tile in cells.</param>
/// <param name="tileId">The ID of the DEM tile.</param>
/// <returns>
/// A tuple representing the minimum cell coordinates (X, Y) of the tile.
/// </returns>
let tileMinCell (tileSize: int) (tileId: DemTileId) : DemGlobalCellCoords =
    let cellX = tileXToCellX tileSize (tileId.TileX |> float)
    let cellY = tileYToCellY tileSize (tileId.TileY |> float)

    (cellX |> Math.Floor |> int, cellY |> Math.Floor |> int)

let findTileFromGlobalCellCoordinates
    tileSize
    (level: DemLevel)
    ((cellX, cellY): DemGlobalCellCoords)
    : DemTileId =
    let tileX = cellXToTileX tileSize cellX |> floor |> int
    let tileY = cellYToTileY tileSize cellY |> floor |> int

    { Level = level
      TileX = tileX
      TileY = tileY }

let inline cellsPerDegree tileSize (level: DemLevel) =
    (float tileSize) / levelFactorFloat level

/// <summary>
/// Converts longitude value (in degrees) to DEM cell X coordinate.
/// </summary>
let inline longitudeToCellX cellsPerDegree (lon: LongitudeDegrees) =
    lon * cellsPerDegree

/// <summary>
/// Converts latitude value (in degrees) to DEM cell Y coordinate.
/// </summary>
let inline latitudeToCellY cellsPerDegree (lat: LatitudeDegrees) =
    lat * cellsPerDegree

/// <summary>
/// Converts cell X coordinates to longitude (in degrees).
/// </summary>
/// <param name="cellsPerDegree">
/// The number of cells per degree, calculated based on the tile size and the
/// DEM level.
/// </param>
/// <param name="cellX">The X coordinate of a cell in the DEM tile.</param>
/// <returns>
/// The longitude (in degrees) corresponding to the cell X coordinate.
/// </returns>
let inline cellXToLongitude
    cellsPerDegree
    (cellX: DemCellFractionalX)
    : LongitudeDegrees =
    cellX / cellsPerDegree

/// <summary>
/// Converts cell Y coordinates to latitude (in degrees).
/// </summary>
/// <param name="cellsPerDegree">
/// The number of cells per degree, calculated based on the tile size and the
/// DEM level.
/// </param>
/// <param name="cellY">The Y coordinate of a cell in the DEM tile.</param>
/// <returns>
/// The latitude (in degrees) corresponding to the cell Y coordinate.
/// </returns>
let inline cellYToLatitude
    cellsPerDegree
    (cellY: DemCellFractionalY)
    : LatitudeDegrees =
    cellY / cellsPerDegree

let inline demTileId level (tileX: DemTileX) (tileY: DemTileY) =
    { Level = DemLevel.fromInt level
      TileX = tileX
      TileY = tileY }

let inline demTileXYId (tileX: DemTileX) (tileY: DemTileY) =
    { Level = DemLevel.fromInt 0
      TileX = tileX
      TileY = tileY }

/// <summary>
/// Construct a DEM tile ID from a fractional tile X and Y coordinates.
/// </summary>
let inline demTileIdFromFractional
    level
    (tileX: DemTileFractionalX)
    (tileY: DemTileFractionalY)
    =
    { Level = DemLevel.fromInt level
      TileX = Math.Floor tileX |> int
      TileY = Math.Floor tileY |> int }

let toDemTileCoords (tileId: DemTileId) : DemTileCoords =
    match tileId.Level.Value with
    | 0 ->
        let lon = tileId.TileX |> DemLongitude.fromInt
        let lat = tileId.TileY |> DemLatitude.fromInt
        { Lon = lon; Lat = lat }
    | _ ->
        invalidOp
            "Cannot convert DEM tile ID with level > 0 to DEM tile coords."

let toTileName (tileId: DemTileId) : DemTileName =
    match tileId.Level.Value with
    | 0 -> tileId.FormatLat2Lon3
    | _ ->
        let lonSign, latSign = tileId.LonLatCardinalDirectionsSigns
        let lonSign = (lonSign |> string).ToLower()
        let latSign = (latSign |> string).ToLower()
        $"l%01d{tileId.Level.Value}%s{lonSign}%02d{abs tileId.TileX}%s{latSign}%02d{abs tileId.TileY}"


/// <summary>
/// Parses a SRTM tile name in HGT format and returns a corresponding
/// SrtmTileCoords.
/// </summary>
/// <param name="tileId">
/// The name of the SRTM tile in HGT format to parse.
/// </param>
/// <returns>
/// A SrtmTileCoords that represents the parsed SRTM tile.
/// </returns>
/// <remarks>
/// The HGT format is a string of 7 characters: the first character is
/// either 'N' or 'S' indicating the latitude direction,
/// followed by 2 digits for the latitude value, then a character 'E' or 'W'
/// indicating the longitude direction, and finally 3 digits for the
/// longitude value. For example, "N46E006".
/// </remarks>
let parseHgtTileName (tileId: string) =
    let latitudeCharSign = tileId.[0]

    let latitudeSign =
        match latitudeCharSign with
        | 'N' -> 1
        | 'S' -> -1
        | _ ->
            raise (
                InvalidOperationException $"Invalid SRTM tile ID: '%s{tileId}'"
            )

    let longitudeCharSign = tileId.[3]

    let longitudeSign =
        match longitudeCharSign with
        | 'W' -> -1
        | 'E' -> 1
        | _ ->
            raise (
                InvalidOperationException $"Invalid SRTM tile ID: '%s{tileId}'"
            )

    let latitudeStr = tileId.[1..2]
    let latitudeInt = Int32.Parse latitudeStr * latitudeSign
    let latitude = DemLatitude.fromInt latitudeInt

    let longitudeStr = tileId.[4..6]
    let longitudeInt = Int32.Parse longitudeStr * longitudeSign
    let longitude = DemLongitude.fromInt longitudeInt

    { Lon = longitude; Lat = latitude }


/// <summary>
/// Parses a DEM tile name in the SRTM format and returns a corresponding SrtmTileId.
/// </summary>
/// <param name="tileName">The name of the DEM tile to parse.</param>
/// <returns>
/// A DemTileId that represents the parsed DEM tile.
/// </returns>
let parseTileName (tileName: DemTileName) : DemTileId =
    match tileName.[0] with
    | 'l' ->
        let level = tileName.[1..1] |> Int32.Parse |> DemLevel.fromInt

        let longitudeSign =
            match tileName.[2..2] with
            | "w" -> -1
            | "e" -> 1
            | _ -> invalidOp "Invalid longitude sign"

        let x = (tileName.[3..4] |> Int32.Parse) * longitudeSign

        let latitudeSign =
            match tileName.[5..5] with
            | "n" -> 1
            | "s" -> -1
            | _ -> invalidOp "Invalid latitude sign"

        let y = (tileName.[6..7] |> Int32.Parse) * latitudeSign

        { Level = level; TileX = x; TileY = y }

    | _ ->
        let tileCoords = parseHgtTileName tileName

        { Level = DemLevel.fromInt 0
          TileX = tileCoords.Lon.Value
          TileY = tileCoords.Lat.Value }


let tileLonLatBounds tileSize (tile: DemTileId) : LonLatBounds =
    let level = tile.Level
    let minCellX, minCellY = tile |> tileMinCell tileSize

    let cellsPerDegree = cellsPerDegree tileSize level
    let minLon = minCellX |> float |> cellXToLongitude cellsPerDegree
    let minLat = minCellY |> float |> cellYToLatitude cellsPerDegree

    let maxLon =
        (minCellX + tileSize) |> float |> cellXToLongitude cellsPerDegree

    let maxLat =
        (minCellY + tileSize) |> float |> cellYToLatitude cellsPerDegree

    { MinLon = minLon
      MinLat = minLat
      MaxLon = maxLon
      MaxLat = maxLat }

/// <summary>
/// Generates a list of DEM tiles that cover a given geographical bounds.
/// </summary>
/// <param name="tileSize">The size of the tile (in cells).</param>
/// <param name="level">The DEM level of the tiles.</param>
/// <param name="bounds">The geographical bounds to cover.</param>
/// <returns>
/// A list of DEM tiles that cover the given geographical bounds.
/// </returns>
/// <remarks>
/// This function calculates the minimum and maximum tile coordinates that
/// cover the given geographical bounds. It then generates a list of all
/// tiles within these coordinates.
/// </remarks>
let boundsToTiles
    tileSize
    (level: DemLevel)
    (bounds: LonLatBounds)
    : DemTileId list =

    let cellsPerDegree = cellsPerDegree tileSize level

    let minTileX =
        bounds.MinLon
        |> longitudeToCellX cellsPerDegree
        |> cellXToTileX tileSize
        |> floor
        |> int

    let minTileY =
        bounds.MinLat
        |> latitudeToCellY cellsPerDegree
        |> cellYToTileY tileSize
        |> floor
        |> int

    let maxTileX =
        (bounds.MaxLon
         |> longitudeToCellX cellsPerDegree
         |> cellXToTileX tileSize
         |> ceil
         |> int)
        - 1

    let maxTileY =
        (bounds.MaxLat
         |> latitudeToCellY cellsPerDegree
         |> cellYToTileY tileSize
         |> ceil
         |> int)
        - 1


    [ for tileY in [ minTileY..maxTileY ] do
          for tileX in [ minTileX..maxTileX ] do
              yield
                  { Level = level
                    TileX = tileX
                    TileY = tileY } ]


let inline heightFromBytes firstByte secondByte =
    let height: int16 = (int16 firstByte) <<< 8 ||| int16 secondByte

    match height with
    | 0x8000s -> DemHeightNone
    | _ -> height


let mbrOfHeightsArrays (heightsArrays: HeightsArray seq) : Raster.Rect =
    match heightsArrays |> Seq.isEmpty with
    | true -> Raster.Rect.Empty
    | false ->
        let minX = (heightsArrays |> Seq.minBy (_.MinX)).MinX
        let minY = (heightsArrays |> Seq.minBy (_.MinY)).MinY
        let maxX = (heightsArrays |> Seq.maxBy (_.MaxX)).MaxX
        let maxY = (heightsArrays |> Seq.maxBy (_.MaxY)).MaxY

        let width = maxX - minX + 1
        let height = maxY - minY + 1

        { MinX = minX
          MinY = minY
          Width = width
          Height = height }

/// <summary>
/// Merges a list of heights arrays into a single heights array whose
/// bounds are pre-determined.
/// </summary>
/// <remarks>
/// If the list is empty or the width or height of the merged heights array
/// is zero, the function returns None.
/// </remarks>
/// <param name="mergedArrayBounds">
/// The bounds of the merged heights array. Everything outside the bounds will
/// be ignored.
/// </param>
/// <param name="heightArrays">A list of heights arrays to be merged</param>
let merge
    (mergedArrayBounds: Raster.Rect)
    (heightArrays: HeightsArray list)
    : HeightsArray option =

    let copyHeightsArray (source: HeightsArray) (dest: HeightsArray) : unit =
        let copyMinX = max source.MinX dest.MinX
        let copyMinY = max source.MinY dest.MinY
        let copyMaxX = min source.MaxX dest.MaxX
        let copyMaxY = min source.MaxY dest.MaxY

        for y in copyMinY..copyMaxY do
            for x in copyMinX..copyMaxX do
                source.heightAt (x, y) |> dest.setHeightAt (x, y)

    match (heightArrays, mergedArrayBounds.Width, mergedArrayBounds.Height) with
    | _, 0, _ -> None
    | _, _, 0 -> None
    | [], _, _ -> None
    | _ ->
        let merged =
            HeightsArray(
                mergedArrayBounds.MinX,
                mergedArrayBounds.MinY,
                mergedArrayBounds.Width,
                mergedArrayBounds.Height,
                EmptyHeightsArray
            )

        heightArrays |> List.iter (fun x -> copyHeightsArray x merged)

        Some merged

/// <summary>
/// Extracts a sub-array from the given array. If the sub-array is outside
/// the bounds of the given array, the missing values are filled with
/// Int16.MinValue.
/// </summary>
let extract
    (extractBounds: Raster.Rect)
    (heightArray: HeightsArray)
    : HeightsArray =

    let getValue (x, y) : DemHeight =
        if
            x >= heightArray.MinX
            && x <= heightArray.MaxX
            && y >= heightArray.MinY
            && y <= heightArray.MaxY
        then
            // a marker that the water body pixel was not yet visited
            heightArray.heightAt (x, y)
        else
            // this is not a water body pixel
            Int16.MinValue

    HeightsArray(
        extractBounds.MinX,
        extractBounds.MinY,
        extractBounds.Width,
        extractBounds.Height,
        HeightsArrayInitializer2D getValue
    )


/// <summary>
/// Provides a function to fetch a heights array from a sequence of DEM tiles,
/// using a specified DEM tile reader.
/// </summary>
let fetchDemHeights
    (readDemTile: DemTileReader)
    (tilesToUse: DemTileId seq)
    : HeightsArrayMaybeResult =
    let mutable errorMessage = None
    let mutable i = 0

    let tilesArray = tilesToUse |> Seq.toArray
    let mutable heightsArraysToMerge = []

    while i < tilesArray.Length && Option.isNone errorMessage do
        let tileCoords = tilesArray.[i]
        let tileLoadResult = readDemTile tileCoords

        match tileLoadResult with
        | Ok(Some heightsArray) ->
            heightsArraysToMerge <- heightsArray :: heightsArraysToMerge
            ()
        | Error message ->
            errorMessage <- Some message
            ()
        | _ -> ()

        i <- i + 1

    match errorMessage with
    | Some error -> Error error
    | _ ->
        let mergedRect = heightsArraysToMerge |> mbrOfHeightsArrays

        Ok(merge mergedRect heightsArraysToMerge)



/// <summary>
/// Maps the values of a raster heights array using a specified function.
/// </summary>
let mapRasterValues mapFunc (heightsArray: HeightsArray) =
    let mappedCells = heightsArray.Cells |> Array.map mapFunc

    HeightsArray(
        heightsArray.MinX,
        heightsArray.MinY,
        heightsArray.Width,
        heightsArray.Height,
        HeightsArrayDirectImport mappedCells
    )


/// <summary>
/// Calculates the sum of the values of the 8 cells around the cell at (x, y)
/// and the cell itself.
/// </summary>
/// <remarks>
/// Returns a tuple of the sum of the values of the 9 cells and the value of
/// the central cell.
/// </remarks>
let sumCells9 (x: DemCellX) (y: DemCellY) (heightsArray: HeightsArray) =
    let rasterWidth = heightsArray.Width
    let rasterWidth2 = rasterWidth * 2

    let startingIndex =
        (y - 1 - heightsArray.MinY) * rasterWidth + x - 1 - heightsArray.MinX

    let rowCellsIndexes =
        [| 0
           1
           2
           rasterWidth
           rasterWidth + 1
           rasterWidth + 2
           rasterWidth2
           rasterWidth2 + 1
           rasterWidth2 + 2 |]

    let sum =
        rowCellsIndexes
        |> Array.sumBy (fun i -> heightsArray.Cells[startingIndex + i])

    sum, heightsArray.Cells[startingIndex + rasterWidth + 1]

/// <summary>
/// Construct a dictionary of all values present in the heights array, with the
/// value as the key and the number of occurrences as the value.
/// </summary>
let analyzeHeightsArray (heightsArray: HeightsArray) =
    let values = heightsArray.Cells |> Array.groupBy id
    values |> Array.map (fun (k, v) -> k, v.Length) |> Map.ofArray
