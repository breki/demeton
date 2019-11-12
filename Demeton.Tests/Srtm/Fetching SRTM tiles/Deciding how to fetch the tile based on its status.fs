module Tests.Srtm.``Fetching SRTM tiles``.``Deciding how to fetch the tile based on its status``

open Demeton.DemTypes
open Demeton.Srtm.Types
open Demeton.Srtm.Fetch

open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper

type SrtmFetchCommand =
    | NoTile
    | ConvertTileFromHgt of SrtmTileCoords 
    | CreateFromLowerTiles of SrtmTileCoords 
    | ReadCachedTile of SrtmTileCoords

let getCommandBasedOnStatus 
    (tileCoords: SrtmTileCoords) (tileStatus: SrtmTileStatus)
    : SrtmFetchCommand =
    match (tileStatus, tileCoords.Level.Value) with
    | (NotExists, _) -> NoTile
    | (NotCached, level) when level = 0 -> ConvertTileFromHgt tileCoords
    | (NotCached, _) -> CreateFromLowerTiles tileCoords
    | (Cached, _) -> ReadCachedTile tileCoords

[<Fact>]
let ``Already cached tile should be just read``() =
    let tile = srtmTileCoords 4 12 23

    test <@ getCommandBasedOnStatus tile SrtmTileStatus.Cached
                = (ReadCachedTile tile) @>

[<Fact>]
let ``For tile that does not exist just return "no tile" information``() =
    let tile = srtmTileCoords 4 12 23

    test <@ getCommandBasedOnStatus tile SrtmTileStatus.NotExists
                = NoTile @>

[<Fact>]
let ``For level 0 tile that is not already cached, convert it to PNG and cache it``() =
    let tile = srtmTileCoords 0 12 23

    test <@ getCommandBasedOnStatus tile SrtmTileStatus.NotCached
                = (ConvertTileFromHgt tile) @>

[<Fact>]
let ``For higher level tile that is not already cached, try to create it from lower level tiles``() =
    let tile = srtmTileCoords 3 12 23

    test <@ getCommandBasedOnStatus tile SrtmTileStatus.NotCached
                = (CreateFromLowerTiles tile) @>
 
type SrtmTile = { Coords: SrtmTileCoords; Data: HeightsArray }

type CreateFromLowerTilesCommand = { 
    Parent: SrtmTileCoords; Children: SrtmTileCoords list }

type TileProcessingStatus =
    | DetermineStatus of SrtmTileCoords
    | NoTile of SrtmTileCoords
    | Tile of SrtmTile
    | ConvertTileFromHgt of SrtmTileCoords 
    | ReadCachedTile of SrtmTileCoords
    | CreateFromLowerTiles of CreateFromLowerTilesCommand
