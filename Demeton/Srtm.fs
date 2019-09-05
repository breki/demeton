module Demeton.Srtm

open GeometryTypes
open IOTypes
open DemTypes
open SrtmTypes

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


let latitudeCharSign (latitude: SrtmLatitude) =
    match latitude with
    | x when x.Value >= 0 -> 'N'
    | _ -> 'S'


let longitudeCharSign (longitude: SrtmLongitude) =
    match longitude with
    | x when x.Value >= 0 -> 'E'
    | _ -> 'W'


let tileId (tileCoords: SrtmTileCoords) =
    let latSign = latitudeCharSign tileCoords.Lat
    let lonSign = longitudeCharSign tileCoords.Lon

    sprintf 
        "%c%02d%c%03d" 
        latSign (abs tileCoords.Lat.Value) lonSign (abs tileCoords.Lon.Value)


let parseTileId (tileId: string) =
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


let tileCellMinCoords (tileCoords: SrtmTileCoords) =
    { 
        X = (tileCoords.Lon.Value + 179) * 3600;
        Y = (tileCoords.Lat.Value + 90) * 3600
    }


let readSrtmHeightsFromStream (stream: Stream): DemHeight option seq =

    let readNextHeightFromStream (streamReader: FunctionalStreamReader) =
       let firstByte = streamReader.currentByte()

       match streamReader.moveForward() with
       | false -> raise (InvalidOperationException ("Unexpected end of SRTM heights stream reached."))
       | true -> 
            let secondByte = streamReader.currentByte()
            let height: int16 = (int16)firstByte <<< 8 ||| (int16)secondByte
            match height with
            | 0x8000s -> None
            | _ -> Some (height)

    let streamReader = FunctionalStreamReader(stream)

    seq {
        while streamReader.moveForward()
            do yield readNextHeightFromStream streamReader
    }


let toLocalCacheTileFile 
    (tileCoords: SrtmTileCoords) 
    (localCacheDir: string) : SrtmTileHgtFile =
    let tileHgtFileName = sprintf "%s.hgt" (tileId tileCoords)

    SrtmTileHgtFile (tileCoords, Path.Combine(localCacheDir, tileHgtFileName))


let ensureTilesAreInCache 
    (tiles: SrtmTileCoords list) 
    (localCacheDir: string)
    : SrtmTileHgtFile list =
    tiles 
    |> List.map (fun x -> toLocalCacheTileFile x localCacheDir)


// todo: fetchSrtmHeights need to merge the fetched tiles together
let fetchSrtmHeights 
    (tilesToUse: SrtmTileCoords seq)
    (fetchSrtmTiles: FetchSrtmTiles)
    (readSrtmTile: ReadSrtmTile)
    : HeightsArray option = 
    let srtmTiles = fetchSrtmTiles tilesToUse
    match srtmTiles with
    | tiles when Seq.isEmpty tiles -> None
    | _ -> Some (readSrtmTile (srtmTiles |> Seq.head))
