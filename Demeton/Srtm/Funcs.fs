module Demeton.Srtm.Funcs

open Demeton.Geometry.Common
open Demeton.IOTypes
open Demeton.DemTypes
open Demeton.Srtm.Types

open System
open System.IO

let boundsToTiles (bounds: LonLatBounds) (level: SrtmLevel)
    : SrtmTileCoords list =
    let tileFactor = pown 2 level.Value
    let tileFactorFloat = tileFactor |> float

    let minLon = (bounds.MinLon / tileFactorFloat) |> int
    let maxLon = (ceil (bounds.MaxLon / tileFactorFloat) |> int) - 1
            
    let minLat = (bounds.MinLat / tileFactorFloat) |> int
    let maxLat = (ceil (bounds.MaxLat / tileFactorFloat) |> int) - 1

    [ 
        for lat in [ minLat .. maxLat ] do
            for lon in [ minLon .. maxLon ] do
                yield { 
                        Level = level; 
                        Lon = SrtmLongitude.fromInt (lon * tileFactor); 
                        Lat = SrtmLatitude.fromInt (lat * tileFactor) } 
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
    
let createSrtmTileFromStream tileSize tileCoords stream =
    let srtmHeights = readSrtmHeightsFromStream tileSize stream

    let (tileMinX, tileMinY) = Tile.tileCellMinCoords tileSize tileCoords
        
    HeightsArray(
        tileMinX, 
        tileMinY, 
        tileSize, 
        tileSize, 
        HeightsArrayDirectImport srtmHeights)


let toZippedSrtmTileFile
    (srtmDir: string) 
    (tileCoords: SrtmTileCoords) : SrtmTileFile =
    let zippedTileFileName = 
        sprintf "%s.SRTMGL1.hgt.zip" (Tile.tileId tileCoords)

    { 
        TileCoords = tileCoords; 
        FileName = srtmDir |> Pth.combine zippedTileFileName
    }


let toLocalCacheTileFile 
    (localCacheDir: string) 
    (tileCoords: SrtmTileCoords) : SrtmTileFile =
    let tilePngFileName = sprintf "%s.png" (Tile.tileId tileCoords)
    let levelDirName = 
        tileCoords.Level.Value.ToString(
            System.Globalization.CultureInfo.InvariantCulture)

    { 
        TileCoords = tileCoords;
        FileName = 
            localCacheDir 
            |> Pth.combine levelDirName 
            |> Pth.combine tilePngFileName 
    }


type SrtmPngTileReader = string -> Result<HeightsArray, string>
type SrtmHgtToPngTileConverter = SrtmTileFile -> string -> HeightsArray

let checkSrtmTileCachingStatus
    (srtmDir: string)
    (localCacheDir: string)
    (fileExists: FileSys.FileExistsChecker)
    (tile: SrtmTileCoords)
    = 
    let localTileFile = toLocalCacheTileFile localCacheDir tile

    match fileExists localTileFile.FileName with
    | true -> Tile.CachingStatus.Cached
    | false -> 
        let zippedSrtmTileFile = toZippedSrtmTileFile srtmDir tile

        match fileExists zippedSrtmTileFile.FileName with
        | false -> Tile.CachingStatus.DoesNotExist
        | true -> Tile.CachingStatus.NotCached

let fetchSrtmTile 
    (srtmDir: string)
    (localCacheDir: string)
    (fileExists: FileSys.FileExistsChecker)
    (pngTileReader: SrtmPngTileReader)
    (pngTileConverter: SrtmHgtToPngTileConverter): SrtmTileReader =
    fun tile ->
    let localTileFile = toLocalCacheTileFile localCacheDir tile

    match fileExists localTileFile.FileName with
    | true -> 
        let loadResult = pngTileReader localTileFile.FileName
        match loadResult with
        | Ok heightsArray -> Ok (Some heightsArray)
        | Error message -> Error message
    | false -> 
        let zippedSrtmTileFile = toZippedSrtmTileFile srtmDir tile

        match fileExists zippedSrtmTileFile.FileName with
        | false -> Ok None
        | true -> 
            Ok (Some (pngTileConverter 
                zippedSrtmTileFile 
                localTileFile.FileName))


type SrtmHeightsArrayFetcher = SrtmTileCoords seq -> HeightsArrayResult

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
    | _ -> Ok (Demeton.Dem.merge heightsArraysToMerge)
