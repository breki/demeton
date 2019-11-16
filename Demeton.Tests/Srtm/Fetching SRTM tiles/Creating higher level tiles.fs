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

let decodePng: SrtmPngTileReader = fun _ _ -> Ok tileHeights

// fails only on certain tiles
let decodePngWithFailure: SrtmPngTileReader = 
    fun tileId tileFileName ->
        match tileId.TileX = 3 with
        | false -> Ok tileHeights
        | true -> Error "some error"

let parentTile = srtmTileCoords 0 2 4

[<Fact>]
let ``All children tiles have been read successfully``() =
    let tiles = [ srtmTileId 1 2 -4 ]

    let result = readPngTilesBatch cacheDir decodePng tiles
    test <@ result |> isOk @>

[<Fact>]
let ``Reading of some children tiles have failed``() =
    let tiles = [ 
        srtmTileId 1 2 4 
        srtmTileId 1 3 4 
        srtmTileId 1 4 4 
    ]

    let result = readPngTilesBatch cacheDir decodePngWithFailure tiles
    test <@ result |> isError @>

[<Fact(Skip="todo: implement a new tile coordinate sytem")>]
let ``Creates the parent tile heights array by downsampling children heights``() =
    let tileSize = 36
    let tile = srtmTileId 2 4 -4
    let bufferAroundTile = 1

    let (minChildX, minChildY) = 
        srtmTileId 1 4 -6  |> newTileCellMinCoords tileSize
    let (maxChildX, maxChildY) = 
        srtmTileId 1 8 -2 |> newTileCellMinCoords tileSize
    let childrenHeightsArray = 
        HeightsArray(
            minChildX - bufferAroundTile,
            minChildY - bufferAroundTile, 
            maxChildX - minChildX + bufferAroundTile * 2,
            maxChildY - minChildY + bufferAroundTile * 2,
            HeightsArrayInitializer1D (fun _ -> DemHeight 100))

    let (minParentX, minParentY) = tile |> newTileCellMinCoords tileSize
    let maxParentX = minParentX + tileSize
    let maxParentY = minParentY + tileSize
    
    test <@ minParentX * 2 = minChildX @>
    test <@ minParentY * 2 = minChildY @>
    test <@ maxParentX * 2 = maxChildX @>
    test <@ maxParentY * 2 = maxChildY @>
    
    test <@ childrenHeightsArray.Width = (tileSize + bufferAroundTile) * 2 @>
    test <@ childrenHeightsArray.Height = (tileSize + bufferAroundTile) * 2 @>
    
    let parentHeightsMaybe = 
        downsampleTileHeightsArray tileSize tile (Some childrenHeightsArray)

    let (expectedParentTileX, expectedParentTileY)
        = tile |> newTileCellMinCoords tileSize
    test <@ parentHeightsMaybe |> Option.isSome @>

    let parentHeights = Option.get parentHeightsMaybe
    test <@ parentHeights.MinX = expectedParentTileX @>
    test <@ parentHeights.MinY = expectedParentTileY @>
    test <@ parentHeights.Width = tileSize @>
    test <@ parentHeights.Height = tileSize @>