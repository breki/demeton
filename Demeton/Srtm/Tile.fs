[<RequireQualifiedAccess>]
module Demeton.Srtm.Tile

open Demeton.DemTypes
open Demeton.Srtm.Types

open System

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


let tileCellMinCoords tileSize (tileCoords: SrtmTileCoords)
    : GlobalCellCoords =
    (
        (tileCoords.Lon.Value + 179) * tileSize, 
        (tileCoords.Lat.Value + 90) * tileSize
    )


