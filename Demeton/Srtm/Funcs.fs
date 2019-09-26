module Demeton.Srtm.Funcs

open Demeton.GeometryTypes
open Demeton.IOTypes
open Demeton.DemTypes
open Demeton.Srtm.Types

open System
open System.IO


let boundsToTiles (bounds: Bounds): SrtmTileCoords list =
    let allLons = 
        [ floor bounds.MinLon |> int 
            .. (ceil bounds.MaxLon |> int) - 1]
    let allLats = 
        [ floor bounds.MinLat |> int 
            .. (ceil bounds.MaxLat |> int) - 1]

    [ 
        for lat in allLats do
            for lon in allLons do
                yield { 
                        Lon = SrtmLongitude.fromInt lon; 
                        Lat = SrtmLatitude.fromInt lat; } 
    ]


let inline heightFromBytes firstByte secondByte =
    let height: int16 = (int16)firstByte <<< 8 ||| (int16)secondByte
    match height with
    | 0x8000s -> None
    | _ -> Some (height)


let readSrtmHeightsFromStream (stream: Stream): DemHeight option seq =

    let inline readNextHeightFromStream (streamReader: FunctionalStreamReader) =
       let firstByte = streamReader.currentByte()

       match streamReader.moveForward() with
       | false -> raise (InvalidOperationException ("Unexpected end of SRTM heights stream reached."))
       | true -> 
            let secondByte = streamReader.currentByte()
            heightFromBytes firstByte secondByte 

    let streamReader = FunctionalStreamReader(stream)

    seq {
        while streamReader.moveForward()
            do yield readNextHeightFromStream streamReader
    }

    
let createSrtmTileFromStream tileSize tileCoords stream =
    let heights1DArray = readSrtmHeightsFromStream stream |> Array.ofSeq

    let (tileMinX, tileMinY) = Tile.tileCellMinCoords tileSize tileCoords
        
    let inline heightFrom1DArray ((cx, cy): GlobalCellCoords) =
        let arrayIndex = cx - tileMinX
                        + (cy - tileMinY) * (tileSize + 1)
        heights1DArray.[arrayIndex]

    HeightsArray(tileMinX, tileMinY, tileSize, tileSize, heightFrom1DArray)


let toZippedSrtmTileFile
    (srtmDir: string) 
    (tileCoords: SrtmTileCoords) : SrtmTileFile =
    let zippedTileFileName = 
        sprintf "%s.SRTMGL1.hgt.zip" (Tile.tileId tileCoords)

    { 
        TileCoords = tileCoords; 
        FileName = Path.Combine(srtmDir, zippedTileFileName) 
    }


let toLocalCacheTileFile 
    (localCacheDir: string) 
    (tileCoords: SrtmTileCoords) : SrtmTileFile =
    let tilePngFileName = sprintf "%s.png" (Tile.tileId tileCoords)

    { 
        TileCoords = tileCoords; 
        FileName = Path.Combine(localCacheDir, tilePngFileName) 
    }


let ensureTilesAreInCache 
    (tiles: SrtmTileCoords list) 
    (localCacheDir: string)
    : SrtmTileFile list =
    tiles 
    |> List.map (fun x -> toLocalCacheTileFile localCacheDir x)


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
    (pngTileConverter: SrtmHgtToPngTileConverter)
    (tile: SrtmTileCoords)
    : HeightsArrayResult =
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


let fetchSrtmHeights 
    (tilesToUse: SrtmTileCoords seq)
    (readSrtmTile: SrtmTileReader)
    : HeightsArrayResult = 

    let mutable errorMessage = None
    let mutable i = 0;

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
