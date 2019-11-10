[<RequireQualifiedAccess>]
module Demeton.Srtm.Tile

open Demeton.DemTypes
open Demeton.Srtm.Types

open System

/// <summary>
/// Defines the caching status of a SRTM tile.
/// </summary>
type CachingStatus = 
    /// <summary>
    /// The tile does not exist in the SRTM storage.
    /// </summary>
    DoesNotExist = 0
    /// <summary>
    /// The tile exists in the SRTM storage, but it was not yet imported into
    /// the local cache.
    /// </summary>
    | NotCached = 1
    /// <summary>
    /// The tile was already imported into the local cache.
    /// </summary>
    | Cached = 2

/// <summary>
/// A function that returns the caching status of a specified SRTM tile.
/// </summary>
type CachingStatusChecker = SrtmTileCoords -> CachingStatus

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

    { Level = SrtmLevel.fromInt 0; Lon = longitude; Lat = latitude }


let tileCellMinCoords tileSize (tileCoords: SrtmTileCoords)
    : GlobalCellCoords =
    (
        (tileCoords.Lon.Value + 179) * tileSize, 
        (90 - tileCoords.Lat.Value) * tileSize - (tileSize - 1)
    )

/// <summary>
/// Calculates the global fractional cell X coordinate for the specified
/// longitude.
/// </summary>
/// <remarks>
/// Note that the function uses the cell's center as the rounded 
/// (non-fractional) coordinate, so each cell stretches from -0.5 to 0.5 value 
/// of X.
/// </remarks>
let longitudeToGlobalX (longitude: float) (tileSize: int): float =
    (longitude + 179.) * float tileSize

/// <summary>
/// Calculates the global fractional cell Y coordinate for the specified
/// latitude.
/// </summary>
/// <remarks>
/// Note that the function uses the cell's center as the rounded 
/// (non-fractional) coordinate, so each cell stretches from -0.5 to 0.5 value 
/// of Y.
/// </remarks>
let latitudeToGlobalY (latitude: float) (tileSize: int): float =
    (90. - latitude) * float tileSize


