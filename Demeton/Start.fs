module Demeton

open System.IO

type Bounds = { 
    MinLon: double
    MinLat: double 
    MaxLon: double 
    MaxLat: double
    }

[<StructuredFormatDisplay("SrtmTile ({Lon}, {Lat})")>]
type SrtmTileCoords = { Lon: int; Lat: int }

type SrtmTileHgtFile = SrtmTileCoords * string

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
        "%c%02d%c%03d" latSign tileCoords.Lat lonSign tileCoords.Lon

let toLocalCacheTileFile 
    (tileCoords: SrtmTileCoords) 
    (localCacheDir: string) : SrtmTileHgtFile =
    let tid = tileId(tileCoords)
    let tileHgtFileName = sprintf "%s.hgt" tid

    let tile: SrtmTileHgtFile = 
        (tileCoords, Path.Combine(localCacheDir, tileHgtFileName))

    tile

let ensureTilesAreInCache 
    (tiles: SrtmTileCoords list) 
    (localCacheDir: string)
    : SrtmTileHgtFile list =
    tiles 
    |> List.map (fun x -> toLocalCacheTileFile x localCacheDir)

let hillshade (bounds: Bounds): Stream option =
    let neededTiles = boundsToTiles bounds

    let neededTilesFiles = ensureTilesAreInCache neededTiles

    None
