module Demeton.Srtm

open GeometryTypes
open DemTypes
open SrtmTypes
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
                yield { Lon = lon; Lat = lat; } 
    ]

let latitudeCharSign latitude =
    match latitude with
    | x when x >= 0 -> 'N'
    | _ -> 'S'

let longitudeCharSign longitude =
    match longitude with
    | x when x >= 0 -> 'E'
    | _ -> 'W'

let tileId (tileCoords: SrtmTileCoords) =
    let latSign = latitudeCharSign(tileCoords.Lat)
    let lonSign = longitudeCharSign(tileCoords.Lon)

    sprintf 
        "%c%02d%c%03d" 
        latSign (abs tileCoords.Lat) lonSign (abs tileCoords.Lon)

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
    : HeightArray option = 
    let srtmTiles = fetchSrtmTiles tilesToUse
    match srtmTiles with
    | tiles when Seq.isEmpty tiles -> None
    | _ -> Some (readSrtmTile (srtmTiles |> Seq.head))
