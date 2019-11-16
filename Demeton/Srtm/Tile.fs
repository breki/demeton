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
type CachingStatusChecker = SrtmTileId -> CachingStatus


