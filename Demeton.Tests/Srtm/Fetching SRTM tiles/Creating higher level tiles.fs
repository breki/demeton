module Tests.Srtm.``Fetching SRTM tiles``.``Creating higher level tiles``

open Demeton.Srtm
open Demeton.Srtm.Funcs
open Demeton.Srtm.Fetch
open Demeton.DemTypes

open Xunit
open Swensen.Unquote
open TestHelp
open Tests.Srtm.SrtmHelper

let cacheDir = "somecache"

let tileHeights = 
    HeightsArray(1, 2, 3, 4, HeightsArrayInitializer1D (fun _ -> DemHeightNone))

let decodePng: SrtmPngTileReader = fun _ -> Ok tileHeights

// fails only on certain tiles
let decodePngWithFailure: SrtmPngTileReader = 
    fun tileFileName ->
        match tileFileName.Contains("E003") with
        | false -> Ok tileHeights
        | true -> Error "some error"

let parentTile = srtmTileCoords 0 2 4

[<Fact>]
let ``All children tiles have been read successfully``() =
    let tiles = [ srtmTileCoords 1 2 4 ]

    let result = readPngTilesBatch cacheDir decodePng tiles
    test <@ result |> isOk @>

[<Fact>]
let ``Reading of some children tiles have failed``() =
    let tiles = [ 
        srtmTileCoords 1 2 4 
        srtmTileCoords 1 3 4 
        srtmTileCoords 1 4 4 
    ]

    let result = readPngTilesBatch cacheDir decodePngWithFailure tiles
    test <@ result |> isError @>

[<Fact>]
let ``Creates the parent tile heights array by downsampling children heights``() =
    let tileSize = 36
    let tile = srtmTileCoords 2 4 4

    let (minChildX, minChildY) = 
        srtmTileCoords 1 2 8
        |> Tile.tileCellMinCoords tileSize
    let (maxChildX, maxChildY) = 
        srtmTileCoords 1 10 0
        |> Tile.tileCellMinCoords tileSize
    let childrenHeightsArray = 
        HeightsArray(minChildX, minChildY, 
            maxChildX - minChildX, maxChildY - minChildY,
            HeightsArrayInitializer1D (fun _ -> DemHeight 100))

    let parentHeightsMaybe = 
        downsampleTileHeightsArray tileSize tile (Some childrenHeightsArray)

    let (expectedParentTileX, expectedParentTileY)
        = tile |> Tile.tileCellMinCoords tileSize
    test <@ parentHeightsMaybe |> Option.isSome @>

    let parentHeights = Option.get parentHeightsMaybe
    test <@ parentHeights.MinX = expectedParentTileX @>
    test <@ parentHeights.MinY = expectedParentTileY @>
    test <@ parentHeights.Width = tileSize @>
    test <@ parentHeights.Height = tileSize @>