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

type CollectLowerTilesCommand = { 
    Parent: SrtmTileCoords; Children: SrtmTileCoords[] }

type TileProcessingStatus =
    | DetermineStatus of SrtmTileCoords
    | NoTile of SrtmTileCoords
    | Tile of SrtmTile
    | ConvertTileFromHgt of SrtmTileCoords 
    | ReadCachedTile of SrtmTileCoords
    | CollectLowerTiles of CollectLowerTilesCommand

let pushNewTile tile stack =
    (DetermineStatus tile) :: stack

// todo tests for listChildrenTiles that check edge cases
let listChildrenTiles tile =
    let childLevel = tile.Level.Value - 1
    let childLonLatDelta = pown 2 childLevel
    let childLon0 = tile.Lon.Value - childLonLatDelta
    let childLat0 = tile.Lat.Value - childLonLatDelta

    [|
        for lat in 1 .. 4 do
            for lon in 1 .. 4 do
                yield { 
                    Level = SrtmLevel.fromInt childLevel; 
                    Lon = SrtmLongitude.fromInt 
                        (childLon0 + (lon - 1) * childLonLatDelta)
                    Lat = SrtmLatitude.fromInt 
                        (childLat0 + (lat - 1) * childLonLatDelta) }
    |]

let processNextItem 
    determineTileStatus
    stack =
    match stack with
    | [] -> invalidOp "todo"

    | (DetermineStatus tile) :: rest -> 
        match determineTileStatus tile with
        | NotExists -> (NoTile tile) :: rest
        | Cached -> (ReadCachedTile tile) :: rest
        | NotCached when tile.Level.Value = 0 -> 
            (ConvertTileFromHgt tile) :: rest
        | NotCached ->
            let childrenTiles = listChildrenTiles tile
            let cmd = { Parent = tile; Children = childrenTiles }
            let childrenItems = 
                childrenTiles |> Array.map DetermineStatus |> Array.toList
            List.append 
                childrenItems 
                ((CollectLowerTiles cmd) :: rest)

    | _ -> invalidOp "todo"

[<Fact>]
let ``When a tile does not exist, put NoTile``() =
    let tile = srtmTileCoords 0 10 20

    let resultingStack =
        [] 
        |> pushNewTile tile
        |> processNextItem (fun _ -> NotExists)


    test <@ resultingStack = [ NoTile tile ] @>

[<Fact>]
let ``When a tile is cached, put ReadCachedTile``() =
    let tile = srtmTileCoords 0 10 20

    let resultingStack =
        [] 
        |> pushNewTile tile
        |> processNextItem (fun _ -> Cached)

    test <@ resultingStack = [ ReadCachedTile tile ] @>

[<Fact>]
let ``When a level 0 tile is not cached, put ConvertTileFromHgt``() =
    let tile = srtmTileCoords 0 10 20

    let resultingStack =
        [] 
        |> pushNewTile tile
        |> processNextItem (fun _ -> NotCached)

    test <@ resultingStack = [ ConvertTileFromHgt tile ] @>

[<Fact>]
let ``When a level > 0 tile is not cached, fill the stack with children tiles and put CreateFromLowerTiles after it``() =
    let tile = srtmTileCoords 2 4 8

    let resultingStack =
        [] 
        |> pushNewTile tile
        |> processNextItem (fun _ -> NotCached)

    let childTiles = 
        [| 
            (2, 6); (4, 6); (6, 6); (8, 6) 
            (2, 8); (4, 8); (6, 8); (8, 8) 
            (2, 10); (4, 10); (6, 10); (8, 10) 
            (2, 12); (4, 12); (6, 12); (8, 12) 
        |] 
        |> Array.map (fun (lon, lat) -> srtmTileCoords 1 lon lat)

    let childItems =
        childTiles |> Array.map (fun tile -> DetermineStatus tile)
    let expectedStack = 
        Array.append 
            childItems 
            [| CollectLowerTiles { Parent = tile; Children = childTiles } |]
        |> Array.toList

    test <@ resultingStack = expectedStack @>
