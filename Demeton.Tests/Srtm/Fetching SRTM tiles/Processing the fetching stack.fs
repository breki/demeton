module Tests.Srtm.``Fetching SRTM tiles``.``Processing the fetching stack``

open Demeton.Srtm.Funcs
open Demeton.Srtm.Fetch
open Demeton.DemTypes

open Xunit
open Swensen.Unquote
open TestHelp

let localCacheDir = "somecache"
let srtmDir = "somesrtm"

let someTileHeights = 
    HeightsArray(1, 2, 3, 4, HeightsArrayInitializer1D (fun _ -> DemHeightNone))

// some random commands in the initial state just so we can assert they are
// still there after processing
let initialCommands = [ 
    DetermineStatus (srtmTileId 0 10 20)
    ConvertTileFromHgt (srtmTileId 0 10 20)
]

// some random tiles in the initial state just so we can assert they are
// still there after processing
let initialStackedTiles = [ Some (srtmTileId 0 10 20); None ]

let initialState = (initialCommands, initialStackedTiles)

[<Fact>]
let ``Calling processNextCommand on an empty command stack returns the same state``() =
    let initialState = 
        (([]: TileProcessingCommand list), initialStackedTiles)

    let resultingState = 
        initialState
        |> processNextCommand 
            localCacheDir srtmDir _noCall _noCall _noCall2 _noCall2

    test <@ resultingState = initialState @>

[<Fact>]
let ``When a tile does not exist, puts None in the tiles stack``() =
    let tile = srtmTileId 0 10 20

    let resultingState =
        initialState 
        |> newTileToProcess tile
        |> processNextCommand 
            localCacheDir srtmDir
            (fun _ -> NotExists) _noCall _noCall2 _noCall2

    test <@ resultingState = (initialCommands, None :: initialStackedTiles) @>

[<Fact>]
let ``When a tile is cached, puts it into the tiles stack``() =
    let tile = srtmTileId 0 10 20

    let resultingState =
        initialState 
        |> newTileToProcess tile
        |> processNextCommand 
            localCacheDir srtmDir
            (fun _ -> Cached) _noCall _noCall2 _noCall2

    test <@ resultingState = 
                (initialCommands, Some tile :: initialStackedTiles) @>

[<Fact>]
let ``When a level 0 tile is not cached, puts ConvertTileFromHgt command``() =
    let tile = srtmTileId 0 10 20

    let resultingState =
        initialState 
        |> newTileToProcess tile
        |> processNextCommand 
            localCacheDir srtmDir
            (fun _ -> NotCached) _noCall _noCall2 _noCall2

    test <@ resultingState = 
                (ConvertTileFromHgt tile :: initialCommands, 
                initialStackedTiles) @>

[<Fact>]
let ``When a level > 0 tile is not cached, fills the command stack with children tiles and puts CreateFromLowerTiles after it``() =
    let tile = srtmTileId 2 4 8

    let resultingState =
        initialState 
        |> newTileToProcess tile
        |> processNextCommand 
            localCacheDir srtmDir
            (fun _ -> NotCached) _noCall _noCall2 _noCall2

    let childTiles = 
        [| 
            (7, 15); (8, 15); (9, 15); (10, 15) 
            (7, 16); (8, 16); (9, 16); (10, 16) 
            (7, 17); (8, 17); (9, 17); (10, 17) 
            (7, 18); (8, 18); (9, 18); (10, 18) 
        |] 
        |> Array.map (fun (x, y) -> srtmTileId 1 x y)

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
    let tile = srtmTileId 0 4 8

    let mutable convertWasCalled = false
    let callConvert _ _ _ = 
        convertWasCalled <- true
        Ok someTileHeights

    let resultingState =
        (ConvertTileFromHgt tile :: initialCommands, initialStackedTiles) 
        |> processNextCommand 
            localCacheDir srtmDir _noCall callConvert _noCall2 _noCall2
    test <@ resultingState = 
                (initialCommands, 
                Some tile :: initialStackedTiles) @>
    test <@ convertWasCalled @>

// Converting of HGT tile to PNG can fail, in which case we need to handle this
// error and return it as the next command in the stack.
[<Fact>]
let ``Convert to PNG can fail``() =
    let tile = srtmTileId 0 4 8
    let errorMessage = "some failure"

    let convertThatFails _ _ _ =  Error errorMessage

    let resultingState =
        (ConvertTileFromHgt tile :: initialCommands, initialStackedTiles) 
        |> processNextCommand 
            localCacheDir srtmDir 
            _noCall convertThatFails _noCall2 _noCall2

    let expectedResultingState =
        (Failure errorMessage :: initialCommands, initialStackedTiles)

    test <@ resultingState = expectedResultingState @>

// When create from lower tiles command is received, collect the specified 
// number of tiles (or None) from the stack and call the provided functions
// for constructing and saving the parent tile.
// Put the resulting tile into the tiles stack.
[<Fact>]
let ``When create from lower tiles command is received``() =
    let tile = srtmTileId 2 4 8
    
    let createFromLowerTilesCmd = 
        { Parent = tile; 
            Children = [| srtmTileId 1 4 8; srtmTileId 1 4 9 |] }
        |> CreateFromLowerTiles

    let tilesStack = 
        [ Some (srtmTileId 1 4 8); None ] @ initialStackedTiles

    let constructParentTile _ _ = Ok (Some someTileHeights)

    let mutable tilePngWasCreated = false
    let writeTileToCache _ _ = 
        tilePngWasCreated <- true

    let resultingState =
        (createFromLowerTilesCmd :: initialCommands, tilesStack) 
        |> processNextCommand 
            localCacheDir srtmDir _noCall _noCall 
            constructParentTile writeTileToCache

    test <@ resultingState =
                (initialCommands,
                Some tile :: initialStackedTiles) @>

    // assert that the tile was saved
    test <@ tilePngWasCreated @>

[<Fact>]
let ``Testing the tail recursion``() =
    // we add sufficiently large number of commands into the command stack so
    // we can be sure it will fail if the function is not tail-recursive
    let someTiles = [| for _ in 0 .. 10000 -> srtmTileId 0 1 2 |]

    // push these tiles into the command stack
    let initialState =
        someTiles
        |> Array.rev
        |> Array.fold (fun status tile -> status |> newTileToProcess tile) 
            ([], [])

    // run the processing of the stack recursively
    let (finalCommandStack, finalTilesStack) = 
        processCommandStack
            localCacheDir srtmDir
            // all of the tiles are marked as cached
            (fun _ -> Cached) (fun _ _ _ -> Ok someTileHeights) 
            _noCall2 _noCall2
            initialState

    // there should be no more commands in the stack
    test <@ finalCommandStack = [] @>
    // ensure all of the tiles are in the tiles stack
    test <@ finalTilesStack = 
                (someTiles |> Array.rev 
                |> Array.map Option.Some |> Array.toList) @>

[<Fact>]
let ``Command stack processor should stop on failure and return the error``() =
    let someTiles = [| for _ in 0 .. 10 -> srtmTileId 0 1 2 |]

    // push these tiles into the command stack
    let initialState =
        someTiles
        |> Array.rev
        |> Array.fold (fun status tile -> status |> newTileToProcess tile) 
            ([], [])

    // convert call will fail once
    let mutable convertCallsCount = 0
    let errorMessage = "some error"
    let convertFailsOnSomeCall onCallCount _ _ _ =
        convertCallsCount <- convertCallsCount + 1
        match convertCallsCount = onCallCount + 1 with
        | true -> Error errorMessage
        | false -> Ok someTileHeights

    // 
    let (finalCommandStack, _) = 
        processCommandStack
            localCacheDir srtmDir
            (fun _ -> NotCached) 
            (convertFailsOnSomeCall 4)
            _noCall2 _noCall2
            initialState

    // there should be an error indicator next in the command stack
    test <@ finalCommandStack.IsEmpty |> not @>
    test <@ finalCommandStack.Head = Failure errorMessage @>
