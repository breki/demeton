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


type SrtmPngTileReader = string -> HeightsArray
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
    : HeightsArray option =
    let localTileFile = toLocalCacheTileFile localCacheDir tile

    match fileExists localTileFile.FileName with
    | true -> Some (pngTileReader localTileFile.FileName)
    | false -> 
        let zippedSrtmTileFile = toZippedSrtmTileFile srtmDir tile

        match fileExists zippedSrtmTileFile.FileName with
        | false -> None
        | true -> 
            Some (pngTileConverter 
                zippedSrtmTileFile 
                localTileFile.FileName)


let fetchSrtmHeights 
    (tilesToUse: SrtmTileCoords seq)
    (readSrtmTile: SrtmTileReader)
    : HeightsArray option = 

    let tilesHeightsArrays = 
        tilesToUse 
        |> Seq.map (fun tileCoords -> readSrtmTile tileCoords)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Seq.toList

    Demeton.Dem.merge tilesHeightsArrays
