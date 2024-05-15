module Demeton.Srtm.Funcs

open Demeton.Geometry.Common
open Demeton.IOTypes
open Demeton.DemTypes
open Demeton.Srtm.Types

open FileSys
open System
open System.IO

let inline private levelFactorFloat (level: SrtmLevel) =
    1 <<< level.Value |> float

let inline tileXToCellX (tileSize: int) (tileX: float) =
    tileX * (tileSize |> float)

let inline tileYToCellY (tileSize: int) (tileY: float) =
    tileY * (tileSize |> float)

/// <summary>
/// Converts SRTM cell X coordinate to a fractional tile X coordinate.
/// </summary>
let inline cellXToTileX (tileSize: int) (cellX: float) =
    cellX / (tileSize |> float)

/// <summary>
/// Converts SRTM cell Y coordinate to a fractional tile Y coordinate.
/// </summary>
let inline cellYToTileY (tileSize: int) (cellY: float) =
    cellY / (tileSize |> float)

/// <summary>
/// Calculates the minimum cell coordinates for a given SRTM tile.
/// </summary>
/// <param name="tileSize">The size of the tile in cells.</param>
/// <param name="tileId">The ID of the SRTM tile.</param>
/// <returns>
/// A tuple representing the minimum cell coordinates (X, Y) of the tile.
/// </returns>
let tileMinCell (tileSize: int) (tileId: SrtmTileId) : SrtmTileCellCoordsInt =
    let cellX = tileXToCellX tileSize (tileId.TileX |> float)
    let cellY = tileYToCellY tileSize (tileId.TileY |> float)

    (cellX |> Math.Round |> int, cellY |> Math.Round |> int)

let findTileFromGlobalCoordinates
    tileSize
    (level: SrtmLevel)
    (x, y)
    : SrtmTileId =
    let tileX = cellXToTileX tileSize x |> floor |> int
    let tileY = cellYToTileY tileSize y |> floor |> int

    { Level = level
      TileX = tileX
      TileY = tileY }

let inline cellsPerDegree tileSize (level: SrtmLevel) =
    (float tileSize) / levelFactorFloat level

/// <summary>
/// Converts longitude value (in degrees) to SRTM cell X coordinate.
/// </summary>
let inline longitudeToCellX cellsPerDegree (lon: float) = lon * cellsPerDegree

/// <summary>
/// Converts latitude value (in degrees) to SRTM cell Y coordinate.
/// </summary>
let inline latitudeToCellY cellsPerDegree (lat: float) = -lat * cellsPerDegree

/// <summary>
/// Converts cell X coordinates to longitude (in degrees).
/// </summary>
/// <param name="cellsPerDegree">
/// The number of cells per degree, calculated based on the tile size and the
/// SRTM level.
/// </param>
/// <param name="cellX">The X coordinate of a cell in the SRTM tile.</param>
/// <returns>
/// The longitude (in degrees) corresponding to the cell X coordinate.
/// </returns>
let inline cellXToLongitude cellsPerDegree cellX = cellX / cellsPerDegree

/// <summary>
/// Converts cell Y coordinates to latitude (in degrees).
/// </summary>
/// <param name="cellsPerDegree">
/// The number of cells per degree, calculated based on the tile size and the
/// SRTM level.
/// </param>
/// <param name="cellY">The Y coordinate of a cell in the SRTM tile.</param>
/// <returns>
/// The latitude (in degrees) corresponding to the cell Y coordinate.
/// </returns>
let inline cellYToLatitude cellsPerDegree cellY = -cellY / cellsPerDegree

let inline srtmTileId level tileX tileY =
    { Level = SrtmLevel.fromInt level
      TileX = tileX
      TileY = tileY }

let toSrtmTileCoords (tileId: SrtmTileId) : SrtmTileCoords =
    match tileId.Level.Value with
    | 0 ->
        let lon = tileId.TileX |> SrtmLongitude.fromInt
        let lat = -(tileId.TileY + 1) |> SrtmLatitude.fromInt
        { Lon = lon; Lat = lat }
    | _ ->
        invalidOp
            "Cannot convert SRTM tile ID with level > 0 to SRTM tile coords."

let toHgtTileName (tileCoords: SrtmTileCoords) =
    let latitudeCharSign (latitude: SrtmLatitude) =
        match latitude with
        | x when x.Value >= 0 -> 'N'
        | _ -> 'S'

    let longitudeCharSign (longitude: SrtmLongitude) =
        match longitude with
        | x when x.Value >= 0 -> 'E'
        | _ -> 'W'

    let latSign = latitudeCharSign tileCoords.Lat
    let lonSign = longitudeCharSign tileCoords.Lon

    $"%c{latSign}%02d{abs tileCoords.Lat.Value}%c{lonSign}%03d{abs tileCoords.Lon.Value}"

let toTileName (tileId: SrtmTileId) : SrtmTileName =
    let lonSign tileX = if tileX >= 0 then 'e' else 'w'

    let latSign tileY = if tileY >= 0 then 's' else 'n'

    match tileId.Level.Value with
    | 0 -> tileId |> toSrtmTileCoords |> toHgtTileName
    | _ ->
        $"l%01d{tileId.Level.Value}%c{lonSign tileId.TileX}%02d{abs tileId.TileX}%c{latSign tileId.TileY}%02d{abs tileId.TileY}"

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
    let latitude = SrtmLatitude.fromInt latitudeInt

    let longitudeStr = tileId.[4..6]
    let longitudeInt = Int32.Parse longitudeStr * longitudeSign
    let longitude = SrtmLongitude.fromInt longitudeInt

    { Lon = longitude; Lat = latitude }

/// <summary>
/// Parses a SRTM tile name and returns a corresponding SrtmTileId.
/// </summary>
/// <param name="tileName">The name of the SRTM tile to parse.</param>
/// <returns>
/// A SrtmTileId that represents the parsed SRTM tile.
/// </returns>
let parseTileName (tileName: SrtmTileName) : SrtmTileId =
    match tileName.[0] with
    | 'l' ->
        let level = tileName.[1..1] |> Int32.Parse |> SrtmLevel.fromInt

        let longitudeSign =
            match tileName.[2..2] with
            | "w" -> -1
            | "e" -> 1
            | _ -> invalidOp "Invalid longitude sign"

        let x = (tileName.[3..4] |> Int32.Parse) * longitudeSign

        let latitudeSign =
            match tileName.[5..5] with
            | "n" -> -1
            | "s" -> 1
            | _ -> invalidOp "Invalid latitude sign"

        let y = (tileName.[6..7] |> Int32.Parse) * latitudeSign

        { Level = level; TileX = x; TileY = y }

    | _ ->
        let tileCoords = parseHgtTileName tileName

        { Level = SrtmLevel.fromInt 0
          TileX = tileCoords.Lon.Value
          TileY = -(tileCoords.Lat.Value + 1) }

let tileLonLatBounds tileSize (tile: SrtmTileId) : LonLatBounds =
    let level = tile.Level
    let minCellX, minCellY = tile |> tileMinCell tileSize

    let cellsPerDegree = cellsPerDegree tileSize level
    let minLon = minCellX |> float |> cellXToLongitude cellsPerDegree
    let maxLat = minCellY |> float |> cellYToLatitude cellsPerDegree

    let maxLon =
        (minCellX + tileSize) |> float |> cellXToLongitude cellsPerDegree

    let minLat =
        (minCellY + tileSize) |> float |> cellYToLatitude cellsPerDegree

    { MinLon = minLon
      MinLat = minLat
      MaxLon = maxLon
      MaxLat = maxLat }

/// <summary>
/// Generates a list of SRTM tiles that cover a given geographical bounds.
/// </summary>
/// <param name="tileSize">The size of the tile (in cells).</param>
/// <param name="level">The SRTM level of the tiles.</param>
/// <param name="bounds">The geographical bounds to cover.</param>
/// <returns>
/// A list of SRTM tiles that cover the given geographical bounds.
/// </returns>
/// <remarks>
/// This function calculates the minimum and maximum tile coordinates that
/// cover the given geographical bounds. It then generates a list of all
/// tiles within these coordinates.
/// </remarks>
let boundsToTiles
    tileSize
    (level: SrtmLevel)
    (bounds: LonLatBounds)
    : SrtmTileId list =

    let cellsPerDegree = cellsPerDegree tileSize level

    let minTileX =
        bounds.MinLon
        |> longitudeToCellX cellsPerDegree
        |> cellXToTileX tileSize
        |> floor
        |> int

    let minTileY =
        bounds.MaxLat
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
        (bounds.MinLat
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

/// <summary>
/// Reads HeightsArray data from a SRTM HGT-encoded stream.
/// </summary>
let readSrtmHeightsFromStream tileSize (stream: Stream) : DemHeight[] =

    let inline readNextHeightFromStream (streamReader: FunctionalStreamReader) =
        let firstByte = streamReader.currentByte ()

        match streamReader.moveForward () with
        | false ->
            raise (
                InvalidOperationException(
                    "Unexpected end of SRTM heights stream reached."
                )
            )
        | true ->
            let secondByte = streamReader.currentByte ()
            heightFromBytes firstByte secondByte

    let streamReader = FunctionalStreamReader(stream)

    // Note that the SRTM tile has one pixel/width more of width
    // and height than we need (example: SRTM tiles of tile size 3600 are
    // 3601 of width & height).
    // When reading heights, we skip that additional row and column, so this
    // function returns tileSize*tileSize number of heights.

    let tileSizePlus1 = tileSize + 1

    // Total number of heights we need to read from the stream.
    // Note that this is _not_ the total number of heights in the SRTM tile,
    // since we skip the final column of heights.
    let heightsNeeded = tileSize * tileSizePlus1 - 1

    let arraySize = tileSize * tileSize
    let heightsArray: DemHeight[] = Array.zeroCreate arraySize

    let mutable heightsReadCount = 0
    let mutable heightsWrittenCount = 0

    while heightsReadCount < heightsNeeded && streamReader.moveForward () do
        let heightRead = readNextHeightFromStream streamReader

        // here we check whether the read height belongs to the final column
        // that we should skip
        match heightsReadCount % tileSizePlus1 = tileSize with
        | true -> () // not emitting the final column's height
        | false ->
            heightsArray.[heightsWrittenCount] <- heightRead
            heightsWrittenCount <- heightsWrittenCount + 1

        heightsReadCount <- heightsReadCount + 1

    heightsArray

/// <summary>
/// Reads the <see cref="HeightsArray"/> of a SRTM tile from
/// a HGT file stream.
/// </summary>
let createSrtmTileFromStream tileSize tileId stream =
    let srtmHeights = readSrtmHeightsFromStream tileSize stream

    let cellMinX, cellMinY = tileMinCell tileSize tileId

    HeightsArray(
        cellMinX,
        cellMinY,
        tileSize,
        tileSize,
        HeightsArrayDirectImport srtmHeights
    )


let toZippedSrtmTileFileName (srtmDir: string) (tileCoords: SrtmTileCoords) =
    srtmDir |> Pth.combine $"%s{toHgtTileName tileCoords}.SRTMGL1.hgt.zip"

let toLocalCacheTileFileName
    (localCacheDir: DirectoryName)
    (tileId: SrtmTileId)
    : FileName =
    let tilePngFileName = $"%s{toTileName tileId}.png"

    let levelDirName =
        tileId.Level.Value.ToString(
            System.Globalization.CultureInfo.InvariantCulture
        )

    localCacheDir |> Pth.combine levelDirName |> Pth.combine tilePngFileName


type HeightsArrayPngWriter =
    FileName -> HeightsArray -> Result<HeightsArray, FileSysError>

type SrtmPngTileReader = SrtmTileId -> FileName -> HeightsArrayResult

type SrtmHgtToPngTileConverter =
    SrtmTileId -> string -> string -> HeightsArrayResult

/// <summary>
/// A function that fetches a heights array from a sequence of SRTM tiles.
/// </summary>
type SrtmHeightsArrayFetcher = SrtmTileId seq -> HeightsArrayMaybeResult

/// <summary>
/// Provides a function to fetch a heights array from a sequence of SRTM tiles,
/// using a specified SRTM tile reader.
/// </summary>
let fetchSrtmHeights (readSrtmTile: SrtmTileReader) : SrtmHeightsArrayFetcher =
    fun tilesToUse ->

        let mutable errorMessage = None
        let mutable i = 0

        let tilesArray = tilesToUse |> Seq.toArray
        let mutable heightsArraysToMerge = []

        while i < tilesArray.Length && Option.isNone errorMessage do
            let tileCoords = tilesArray.[i]
            let tileLoadResult = readSrtmTile tileCoords

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
            let mergedRect =
                heightsArraysToMerge |> Demeton.Dem.mbrOfHeightsArrays

            Ok(Demeton.Dem.merge mergedRect heightsArraysToMerge)
