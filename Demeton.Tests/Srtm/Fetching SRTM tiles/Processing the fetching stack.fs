module Tests.Srtm.``Fetching SRTM tiles``.``Processing the fetching stack``

open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Srtm.Fetch

open Xunit
open Swensen.Unquote
open TestHelp

let localCacheDir = "somecache"
let srtmDir = "somesrtm"

let someTileHeights =
    HeightsArray(1, 2, 3, 4, HeightsArrayInitializer1D(fun _ -> DemHeightNone))

// some random commands in the initial state just so we can assert they are
// still there after processing
let initialCommands =
    [ DetermineStatus(demTileId 0 10 20)
      ConvertTileFromHgt(demTileId 0 10 20) ]

// some random tiles in the initial state just so we can assert they are
// still there after processing
let initialStackedTiles = [ Some(demTileId 0 10 20, someTileHeights); None ]

let initialState = (initialCommands, initialStackedTiles)

[<Fact>]
let ``Calling processNextCommand on an empty command stack returns the same state``
    ()
    =
    let initialState = (([]: TileProcessingCommand list), initialStackedTiles)

    let resultingState =
        initialState
        |> processNextCommand
            localCacheDir
            srtmDir
            Should.notBeCalled
            Should.notBeCalled
            Should.notBeCalled2
            Should.notBeCalled2
            Should.notBeCalled2

    test <@ resultingState = initialState @>

[<Fact>]
let ``When a tile does not exist, puts None in the tiles stack`` () =
    let tile = demTileId 0 10 20

    let resultingState =
        initialState
        |> newTileToProcess tile
        |> processNextCommand
            localCacheDir
            srtmDir
            (fun _ -> NotExists)
            Should.notBeCalled
            Should.notBeCalled2
            Should.notBeCalled2
            Should.notBeCalled2

    test <@ resultingState = (initialCommands, None :: initialStackedTiles) @>

[<Fact>]
let ``When a tile is cached, reads it and puts it into the tiles stack`` () =
    let tile = demTileId 0 10 20

    let tileHeights = HeightsArray(1, 2, 3, 4, EmptyHeightsArray)

    let expectReadingOfTile expectedTileId tileId _ =
        test <@ expectedTileId = tileId @>
        Ok tileHeights

    let resultingState =
        initialState
        |> newTileToProcess tile
        |> processNextCommand
            localCacheDir
            srtmDir
            (fun _ -> Cached)
            (expectReadingOfTile tile)
            Should.notBeCalled
            Should.notBeCalled2
            Should.notBeCalled2

    test
        <@
            resultingState = (initialCommands,
                              Some(tile, tileHeights) :: initialStackedTiles)
        @>

[<Fact>]
let ``If reading of a cache tile fails, put error indicator into the stack``
    ()
    =
    let tile = demTileId 0 10 20

    let readingOfTileFails _ _ = Error "some error"

    let resultingState =
        initialState
        |> newTileToProcess tile
        |> processNextCommand
            localCacheDir
            srtmDir
            (fun _ -> Cached)
            readingOfTileFails
            Should.notBeCalled
            Should.notBeCalled2
            Should.notBeCalled2

    test
        <@
            resultingState = (Failure "some error" :: initialCommands,
                              initialStackedTiles)
        @>

[<Fact>]
let ``When a level 0 tile is not cached, puts ConvertTileFromHgt command`` () =
    let tile = demTileId 0 10 20

    let resultingState =
        initialState
        |> newTileToProcess tile
        |> processNextCommand
            localCacheDir
            srtmDir
            (fun _ -> NotCached)
            Should.notBeCalled
            Should.notBeCalled2
            Should.notBeCalled2
            Should.notBeCalled2

    test
        <@
            resultingState = (ConvertTileFromHgt tile :: initialCommands,
                              initialStackedTiles)
        @>

[<Fact>]
let ``When a level > 0 tile is not cached, fills the command stack with children tiles and puts CreateFromLowerTiles after it``
    ()
    =
    let tile = demTileId 2 4 8

    let resultingState =
        initialState
        |> newTileToProcess tile
        |> processNextCommand
            localCacheDir
            srtmDir
            (fun _ -> NotCached)
            Should.notBeCalled
            Should.notBeCalled2
            Should.notBeCalled2
            Should.notBeCalled2

    let childTiles =
        [| (8, 16); (9, 16); (8, 17); (9, 17) |]
        |> Array.map (fun (x, y) -> demTileId 1 x y)

    let childItems = childTiles |> Array.map DetermineStatus

    let expectedCommands =
        Array.append
            childItems
            [| CreateFromLowerTiles { Parent = tile; Children = childTiles } |]
        |> Array.toList

    test
        <@
            resultingState = (expectedCommands @ initialCommands,
                              initialStackedTiles)
        @>

// When convert from HGT command is received, calls the converter. If the
// converted produces a tile, puts its tile ID in the tiles stack.
[<Fact>]
let ``When convert from HGT command is received`` () =
    let tile = demTileId 0 4 8

    let mutable convertWasCalled = false

    let convertProducesSomeTile _ _ _ =
        convertWasCalled <- true
        Ok someTileHeights

    let resultingState =
        (ConvertTileFromHgt tile :: initialCommands, initialStackedTiles)
        |> processNextCommand
            localCacheDir
            srtmDir
            Should.notBeCalled
            Should.notBeCalled2
            convertProducesSomeTile
            Should.notBeCalled2
            Should.notBeCalled2

    test
        <@
            resultingState = (initialCommands,
                              Some(tile, someTileHeights) :: initialStackedTiles)
        @>

    test <@ convertWasCalled @>


// Converting of HGT tile to PNG can fail, in which case we need to handle this
// error and return it as the next command in the stack.
[<Fact>]
let ``Convert to PNG can fail`` () =
    let tile = demTileId 0 4 8
    let errorMessage = "some failure"

    let convertThatFails _ _ _ = Error errorMessage

    let resultingState =
        (ConvertTileFromHgt tile :: initialCommands, initialStackedTiles)
        |> processNextCommand
            localCacheDir
            srtmDir
            Should.notBeCalled
            Should.notBeCalled2
            convertThatFails
            Should.notBeCalled2
            Should.notBeCalled2

    let expectedResultingState =
        (Failure errorMessage :: initialCommands, initialStackedTiles)

    test <@ resultingState = expectedResultingState @>

// When create from lower tiles command is received, collects the specified
// number of tiles (or None) from the stack and calls the provided functions
// for constructing and saving the parent tile. If the constructor function
// returns an actual tile (and not None), put the resulting tile into the
// tiles stack.
[<Fact>]
let ``When create from lower tiles command is received and create returns tile``
    ()
    =
    let tile = demTileId 2 4 8

    let createFromLowerTilesCmd =
        { Parent = tile
          Children = [| demTileId 1 4 8; demTileId 1 4 9 |] }
        |> CreateFromLowerTiles

    let tilesStack =
        [ Some(demTileId 1 4 8, someTileHeights); None ] @ initialStackedTiles

    let constructParentTileReturnsSomeTile _ _ _ = Some someTileHeights

    let mutable tilePngWasCreated = false

    let writeTileToCache tile _ =
        tilePngWasCreated <- true
        Some(tile, someTileHeights) |> Ok

    let resultingState =
        (createFromLowerTilesCmd :: initialCommands, tilesStack)
        |> processNextCommand
            localCacheDir
            srtmDir
            Should.notBeCalled
            Should.notBeCalled2
            Should.notBeCalled
            constructParentTileReturnsSomeTile
            writeTileToCache

    test
        <@
            resultingState = (initialCommands,
                              Some(tile, someTileHeights) :: initialStackedTiles)
        @>

    // assert that the tile was saved
    test <@ tilePngWasCreated @>

// When create from lower tiles command is received, collects the specified
// number of tiles (or None) from the stack and calls the provided functions
// for constructing and saving the parent tile. If the constructor
// returns None (which happens when there is no data for that tile),
// it puts None in the tiles stack.
[<Fact>]
let ``When create from lower tiles command is received and create returns None``
    ()
    =
    let tile = demTileId 2 4 8

    let createFromLowerTilesCmd =
        { Parent = tile
          Children = [| demTileId 1 4 8; demTileId 1 4 9 |] }
        |> CreateFromLowerTiles

    let tilesStack =
        [ Some(demTileId 1 4 8, someTileHeights); None ] @ initialStackedTiles

    let constructParentTileReturnsNone _ _ _ = None

    let mutable tilePngWasCreated = false

    let writeTileToCache _ _ =
        tilePngWasCreated <- true
        Ok None

    let resultingState =
        (createFromLowerTilesCmd :: initialCommands, tilesStack)
        |> processNextCommand
            localCacheDir
            srtmDir
            Should.notBeCalled
            Should.notBeCalled2
            Should.notBeCalled
            constructParentTileReturnsNone
            writeTileToCache

    test <@ resultingState = (initialCommands, None :: initialStackedTiles) @>

    // assert that the tile was saved
    test <@ tilePngWasCreated @>

[<Fact>]
let ``Testing the tail recursion`` () =
    // we add sufficiently large number of commands into the command stack so
    // we can be sure it will fail if the function is not tail-recursive
    let someTiles = [| for _ in 0..10000 -> demTileId 0 1 2 |]

    // push these tiles into the command stack
    let initialState =
        someTiles
        |> Array.rev
        |> Array.fold
            (fun status tile -> status |> newTileToProcess tile)
            ([], [])

    // run the processing of the stack recursively
    let (finalCommandStack, finalTilesStack) =
        processCommandStack
            localCacheDir
            srtmDir
            // all of the tiles are marked as cached
            (fun _ -> Cached)
            (fun _ _ -> Ok someTileHeights)
            (fun _ _ _ -> Ok someTileHeights)
            Should.notBeCalled2
            Should.notBeCalled2
            initialState

    // there should be no more commands in the stack
    test <@ finalCommandStack |> Seq.isEmpty @>
    // ensure all of the tiles are in the tiles stack
    test
        <@
            finalTilesStack = (someTiles
                               |> Array.rev
                               |> Array.map (fun tile ->
                                   Option.Some(tile, someTileHeights))
                               |> Array.toList)
        @>

[<Fact>]
let ``Command stack processor should stop on failure and return the error`` () =
    let someTiles = [| for _ in 0..10 -> demTileId 0 1 2 |]

    // push these tiles into the command stack
    let initialState =
        someTiles
        |> Array.rev
        |> Array.fold
            (fun status tile -> status |> newTileToProcess tile)
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
            localCacheDir
            srtmDir
            (fun _ -> NotCached)
            Should.notBeCalled2
            (convertFailsOnSomeCall 4)
            Should.notBeCalled2
            Should.notBeCalled2
            initialState

    // there should be an error indicator next in the command stack
    test <@ finalCommandStack.IsEmpty |> not @>
    test <@ finalCommandStack.Head = Failure errorMessage @>
