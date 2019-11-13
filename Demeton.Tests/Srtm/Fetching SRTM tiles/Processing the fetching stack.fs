module Tests.Srtm.``Fetching SRTM tiles``.``Processing the fetching stack``

open Demeton.Srtm.Types
open Demeton.Srtm.Fetch

open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper

type CreateFromLowerTiles = { 
    Parent: SrtmTileCoords; Children: SrtmTileCoords[] }

type TileProcessingCommand =
    | DetermineStatus of SrtmTileCoords
    | ConvertTileFromHgt of SrtmTileCoords 
    | CreateFromLowerTiles of CreateFromLowerTiles

type TileProcessingCommandStack = TileProcessingCommand list

type TileInStack = SrtmTileCoords option
type TilesStack = TileInStack list

type TileFetchingState = (TileProcessingCommandStack * TilesStack)

let newTileToProcess tile ((stack, tilesBuffer): TileFetchingState):
    TileFetchingState =
    ((DetermineStatus tile) :: stack, tilesBuffer)

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

let fetchFirstNOfTiles tilesCount = List.splitAt tilesCount

let processNextCommand 
    determineTileStatus
    convertFromHgt
    createFromLowerTiles
    ((commandStack, tilesStack): TileFetchingState)
    : TileFetchingState =
    match commandStack with
    | [] -> invalidOp "todo"

    | DetermineStatus tile :: remainingCommands -> 
        match determineTileStatus tile with
        | NotExists -> (remainingCommands, None :: tilesStack)
        | Cached -> (remainingCommands, Some tile :: tilesStack)
        | NotCached when tile.Level.Value = 0 -> 
            ((ConvertTileFromHgt tile) :: remainingCommands, tilesStack)
        | NotCached ->
            let childrenTiles = listChildrenTiles tile
            let cmd = { Parent = tile; Children = childrenTiles }
            let childrenCommands = 
                childrenTiles |> Array.map DetermineStatus |> Array.toList
            let updatedStack = 
                List.append 
                    childrenCommands 
                    ((CreateFromLowerTiles cmd) :: remainingCommands)
            (updatedStack, tilesStack)

    | ConvertTileFromHgt tile :: remainingCommands ->
        convertFromHgt tile |> ignore
        (remainingCommands, Some tile :: tilesStack)

    | CreateFromLowerTiles parameters :: remainingCommands ->
        let tile = parameters.Parent
       
        // fetch the required number of tiles from the tiles stack
        let lowerTilesNumber = parameters.Children.Length
        let (lowerTiles, remainingTilesInStack) = 
            tilesStack |> fetchFirstNOfTiles lowerTilesNumber

        let lowerTilesWithoutNone =
            lowerTiles
            |> List.filter Option.isSome
            |> List.map Option.get

        createFromLowerTiles tile lowerTilesWithoutNone |> ignore
        (remainingCommands, Some tile :: remainingTilesInStack)


// some random commands in the initial state just so we can assert they are
// still there after processing
let initialCommands = [ 
    DetermineStatus (srtmTileCoords 0 10 20)
    ConvertTileFromHgt (srtmTileCoords 0 10 20)
]

// some random tiles in the initial state just so we can assert they are
// still there after processing
let initialStackedTiles = [ Some (srtmTileCoords 0 10 20); None ]

let initialState = (initialCommands, initialStackedTiles)

let dontCareStatus _ = invalidOp "should not be called"
let dontCallConvert _ = invalidOp "should not be called"
let dontCallCreate _ _ = invalidOp "should not be called"

[<Fact>]
let ``When a tile does not exist, puts None in the tiles stack``() =
    let tile = srtmTileCoords 0 10 20

    let resultingState =
        initialState 
        |> newTileToProcess tile
        |> processNextCommand 
            (fun _ -> NotExists) dontCallConvert dontCallCreate

    test <@ resultingState = (initialCommands, None :: initialStackedTiles) @>

[<Fact>]
let ``When a tile is cached, puts it into the tiles stack``() =
    let tile = srtmTileCoords 0 10 20

    let resultingState =
        initialState 
        |> newTileToProcess tile
        |> processNextCommand 
            (fun _ -> Cached) dontCallConvert dontCallCreate

    test <@ resultingState = 
                (initialCommands, Some tile :: initialStackedTiles) @>

[<Fact>]
let ``When a level 0 tile is not cached, puts ConvertTileFromHgt command``() =
    let tile = srtmTileCoords 0 10 20

    let resultingState =
        initialState 
        |> newTileToProcess tile
        |> processNextCommand 
            (fun _ -> NotCached) dontCallConvert dontCallCreate

    test <@ resultingState = 
                (ConvertTileFromHgt tile :: initialCommands, 
                initialStackedTiles) @>

[<Fact>]
let ``When a level > 0 tile is not cached, fills the command stack with children tiles and puts CreateFromLowerTiles after it``() =
    let tile = srtmTileCoords 2 4 8

    let resultingState =
        initialState 
        |> newTileToProcess tile
        |> processNextCommand 
            (fun _ -> NotCached) dontCallConvert dontCallCreate

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
    let expectedCommands = 
        Array.append 
            childItems 
            [| CreateFromLowerTiles { Parent = tile; Children = childTiles } |]
        |> Array.toList

    test <@ resultingState = 
                (expectedCommands @ initialCommands, initialStackedTiles) @>

// When convert from HGT command is received, calls the converter and puts 
// the result in the tiles stack.
[<Fact>]
let ``When convert from HGT command is received``() =
    let tile = srtmTileCoords 2 4 8

    let mutable convertWasCalled = false
    let callConvert _ = convertWasCalled <- true

    let resultingState =
        (ConvertTileFromHgt tile :: initialCommands, initialStackedTiles) 
        |> processNextCommand dontCareStatus callConvert dontCallCreate
    test <@ resultingState = 
                (initialCommands, 
                Some tile :: initialStackedTiles) @>
    test <@ convertWasCalled @>

// When create from lower tiles command is received, collect the specified 
// number of tiles (or None) from the stack and call the provided method.
// Put the resulting tile into the tiles stack.
[<Fact>]
let ``When create from lower tiles command is received``() =
    let tile = srtmTileCoords 2 4 8
    
    let createFromLowerTilesCmd = 
        { Parent = tile; 
            Children = [| srtmTileCoords 1 4 8; srtmTileCoords 1 4 9 |] }
        |> CreateFromLowerTiles

    let tilesStack = 
        [ Some (srtmTileCoords 1 4 8); None ] @ initialStackedTiles

    let mutable createWasCalled = false
    let callCreate _ _ = createWasCalled <- true

    let resultingState =
        (createFromLowerTilesCmd :: initialCommands, tilesStack) 
        |> processNextCommand dontCareStatus dontCallConvert callCreate

    test <@ resultingState =
                (initialCommands,
                Some tile :: initialStackedTiles) @>
    test <@ createWasCalled @>
