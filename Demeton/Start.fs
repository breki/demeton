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

type SrtmTileHgtFile = SrtmTileHgtFile of SrtmTileCoords * string

type DemHeight = DemHeight of int

type NoHeight = NoHeight of unit

type DemCell =
    | DemHeight
    | NoHeight

type DemData(width, height) =
    member this.Cells = Array2D.create width height NoHeight

type FetchSrtmTiles = SrtmTileCoords seq -> SrtmTileHgtFile seq

type ReadSrtmTile = SrtmTileHgtFile -> DemData

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

let fetchSrtmDemData 
    (tilesToUse: SrtmTileCoords seq)
    (fetchSrtmTiles: FetchSrtmTiles)
    (readSrtmTile: ReadSrtmTile)
    : DemData option = 
    let srtmTiles = fetchSrtmTiles tilesToUse

    None

let hillshade (bounds: Bounds): Stream option =
    let neededTiles = boundsToTiles bounds

    let neededTilesFiles = ensureTilesAreInCache neededTiles

    None
