module Demeton.Srtm.Funcs

open Demeton.Geometry.Common
open Demeton.IOTypes
open Demeton.DemTypes
open Demeton.Srtm.Types

open System
open System.IO

let private levelFactorFloat (level: SrtmLevel) =
    1 <<< level.Value |> float

let tileXToCellX (tileSize: int) (tileX: float) =
    tileX * (tileSize |> float)

let tileYToCellY (tileSize: int) (tileY: float) =
    tileY * (tileSize |> float)

let cellXToTileX (tileSize: int) (cellX: float) =
    cellX / (tileSize |> float)

let cellYToTileY (tileSize: int) (cellY: float) =
    cellY / (tileSize |> float)

let tileMinCell (tileSize: int) (tileId: SrtmTileId)
    : SrtmTileCellCoordsInt =
    let cellX =
        tileXToCellX tileSize (tileId.TileX |> float)
    let cellY =
        tileYToCellY tileSize (tileId.TileY |> float)
    
    (cellX |> System.Math.Round |> int, cellY |> System.Math.Round |> int)

let findTileFromGlobalCoordinates 
    tileSize (level: SrtmLevel) (x, y): SrtmTileId =
    let tileX = cellXToTileX tileSize x |> floor |> int
    let tileY = cellYToTileY tileSize y |> floor |> int
       
    { Level = level; TileX = tileX; TileY = tileY }

let private cellsPerDegree tileSize (level: SrtmLevel) 
    = (float tileSize) / levelFactorFloat level

let longitudeToCellX tileSize (level: SrtmLevel) (lon: float) =
    lon * cellsPerDegree tileSize level

let latitudeToCellY tileSize (level: SrtmLevel) (lat: float) =
    -lat * cellsPerDegree tileSize level

let cellXToLongitude tileSize (level: SrtmLevel) cellX =
    cellX / cellsPerDegree tileSize level

let cellYToLatitude tileSize (level: SrtmLevel) cellY =
    -cellY / cellsPerDegree tileSize level

let srtmTileId level tileX tileY = 
    { Level = SrtmLevel.fromInt level; TileX = tileX; TileY = tileY }

let toSrtmTileCoords (tileId: SrtmTileId): SrtmTileCoords =
    match tileId.Level.Value with
    | 0 -> 
        let lon = tileId.TileX |> SrtmLongitude.fromInt
        let lat = -(tileId.TileY + 1) |> SrtmLatitude.fromInt
        { Lon = lon; Lat = lat }
    | _ ->
        invalidOp 
            "Cannot convert SRTM tile ID with level > 0 to SRTM tile coords."

type SrtmTileName = string

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

    sprintf 
        "%c%02d%c%03d" 
        latSign (abs tileCoords.Lat.Value) lonSign (abs tileCoords.Lon.Value)

let toTileName (tileId: SrtmTileId): SrtmTileName =
    let lonSign tileX = if tileX >= 0 then 'e' else 'w'

    let latSign tileY = if tileY >= 0 then 's' else 'n'

    match tileId.Level.Value with
    | 0 -> tileId |> toSrtmTileCoords |> toHgtTileName
    | _ -> 
        sprintf 
            "l%01d%c%02d%c%02d" 
            tileId.Level.Value
            (lonSign tileId.TileX) (abs tileId.TileX)
            (latSign tileId.TileY) (abs tileId.TileY) 

let parseHgtTileName (tileId: string) =
    let latitudeCharSign = tileId.[0]
    let latitudeSign = 
        match latitudeCharSign with
        | 'N' -> 1
        | 'S' -> -1
        | _ -> raise(InvalidOperationException
                (sprintf "Invalid SRTM tile ID: '%s'" tileId))

    let longitudeCharSign = tileId.[3]

    let longitudeSign = 
        match longitudeCharSign with
        | 'W' -> -1
        | 'E' -> 1
        | _ -> raise(InvalidOperationException
                (sprintf "Invalid SRTM tile ID: '%s'" tileId))

    let latitudeStr = tileId.[1..2]
    let latitudeInt = Int32.Parse latitudeStr * latitudeSign
    let latitude = SrtmLatitude.fromInt latitudeInt

    let longitudeStr = tileId.[4..6]
    let longitudeInt = Int32.Parse longitudeStr * longitudeSign
    let longitude = SrtmLongitude.fromInt longitudeInt

    { Lon = longitude; Lat = latitude }

let parseTileName (tileName: SrtmTileName): SrtmTileId =
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
        { Level = SrtmLevel.fromInt 0;
            TileX = tileCoords.Lon.Value; 
            TileY = -(tileCoords.Lat.Value + 1) }

let tileLonLatBounds tileSize (tile: SrtmTileId): LonLatBounds =
    let level = tile.Level
    let (minCellX, minCellY) = tile |> tileMinCell tileSize
    let minLon = minCellX |> float |> cellXToLongitude tileSize level
    let maxLat = minCellY |> float |> cellYToLatitude tileSize level
    let maxLon = 
        (minCellX + tileSize) |> float |> cellXToLongitude tileSize level
    let minLat = 
        (minCellY + tileSize) |> float |> cellYToLatitude tileSize level

    { MinLon = minLon; MinLat = minLat; MaxLon = maxLon; MaxLat = maxLat }

let boundsToTiles 
    tileSize (level: SrtmLevel) (bounds: LonLatBounds) : SrtmTileId list =
    let minTileX = 
        bounds.MinLon 
        |> longitudeToCellX tileSize level
        |> cellXToTileX tileSize |> floor |> int

    let minTileY = 
        bounds.MaxLat 
        |> latitudeToCellY tileSize level
        |> cellYToTileY tileSize |> floor |> int

    let maxTileX = 
        (bounds.MaxLon
        |> longitudeToCellX tileSize level
        |> cellXToTileX tileSize |> ceil |> int) - 1

    let maxTileY = 
        (bounds.MinLat
        |> latitudeToCellY tileSize level
        |> cellYToTileY tileSize |> ceil |> int) - 1

    [ 
        for tileY in [ minTileY .. maxTileY ] do
            for tileX in [ minTileX .. maxTileX ] do
                yield { Level = level; TileX = tileX; TileY = tileY } 
    ]


let inline heightFromBytes firstByte secondByte =
    let height: int16 = (int16 firstByte) <<< 8 ||| int16 secondByte
    match height with
    | 0x8000s -> DemHeightNone
    | _ -> height


let readSrtmHeightsFromStream tileSize (stream: Stream): DemHeight[] =

    let inline readNextHeightFromStream (streamReader: FunctionalStreamReader) =
       let firstByte = streamReader.currentByte()

       match streamReader.moveForward() with
       | false -> 
        raise (InvalidOperationException 
                ("Unexpected end of SRTM heights stream reached."))
       | true -> 
            let secondByte = streamReader.currentByte()
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
    while heightsReadCount < heightsNeeded && streamReader.moveForward() do
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
    
type ZippedSrtmTileReader = int -> SrtmTileId -> Stream -> HeightsArray

let createSrtmTileFromStream: ZippedSrtmTileReader = 
    fun tileSize tileId stream ->
    let srtmHeights = readSrtmHeightsFromStream tileSize stream

    let (cellMinX, cellMinY) = tileMinCell tileSize tileId
        
    HeightsArray(
        cellMinX, 
        cellMinY, 
        tileSize, 
        tileSize, 
        HeightsArrayDirectImport srtmHeights)


let toZippedSrtmTileFileName
    (srtmDir: string) 
    (tileCoords: SrtmTileCoords) =
    srtmDir
    |> Pth.combine 
        (sprintf "%s.SRTMGL1.hgt.zip" (toHgtTileName tileCoords))

let toLocalCacheTileFileName 
    (localCacheDir: FileSys.DirectoryName) 
    (tileId: SrtmTileId): FileSys.FileName =
    let tilePngFileName = sprintf "%s.png" (toTileName tileId)
    let levelDirName = 
        tileId.Level.Value.ToString(
            System.Globalization.CultureInfo.InvariantCulture)

    localCacheDir 
    |> Pth.combine levelDirName 
    |> Pth.combine tilePngFileName 


type HeightsArrayPngWriter = FileSys.FileName -> HeightsArray -> HeightsArray

type SrtmPngTileReader =
    SrtmTileId -> FileSys.FileName -> Result<HeightsArray, string>
    
type SrtmHgtToPngTileConverter = 
    SrtmTileId -> string -> string -> Result<HeightsArray, string>

type SrtmHeightsArrayFetcher = SrtmTileId seq -> HeightsArrayResult

let fetchSrtmHeights 
    (readSrtmTile: SrtmTileReader): SrtmHeightsArrayFetcher =
    fun tilesToUse ->

    let mutable errorMessage = None
    let mutable i = 0

    let tilesArray = tilesToUse |> Seq.toArray
    let mutable heightsArraysToMerge = []
    while i < tilesArray.Length && Option.isNone errorMessage do
        let tileCoords = tilesArray.[i]
        let tileLoadResult = readSrtmTile tileCoords

        match tileLoadResult with
        | Ok (Some heightsArray) ->
            heightsArraysToMerge <- heightsArray :: heightsArraysToMerge
            ignore()
        | Error message ->
            errorMessage <- Some message
            ignore()
        | _ -> ()

        i <- i + 1

    match errorMessage with
    | Some error -> Error error
    | _ ->
        let mergedRect =
            heightsArraysToMerge |> Demeton.Dem.mbrOfHeightsArrays
        Ok (Demeton.Dem.merge mergedRect heightsArraysToMerge)
