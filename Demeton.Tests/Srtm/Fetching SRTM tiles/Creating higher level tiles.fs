module Tests.Srtm.``Fetching SRTM tiles``.``Creating higher level tiles``

open Demeton.Srtm.Funcs
open Demeton.Srtm.Fetch
open Demeton.DemTypes

open System

open Xunit
open Swensen.Unquote
open TestHelp
open Demeton.Srtm.Types
open Demeton.Geometry.Common

[<Fact>]
let ``Correctly calculates the list of needed children for level 1 (case 1)``() =
    let expectedChildren =
        [| 
            (-1, -1); (0, -1); (1, -1); (2, -1); 
            (-1, 0); (0, 0); (1, 0); (2, 0); 
            (-1, 1); (0, 1); (1, 1); (2, 1); 
            (-1, 2); (0, 2); (1, 2); (2, 2);
        |]
        |> Array.map (fun (x, y) -> srtmTileId 0 x y)
    
    test <@ srtmTileId 1 0 0 |> listChildrenTiles = expectedChildren @>

[<Fact>]
let ``Correctly calculates the list of needed children for level 1 (case 2)``() =
    let tileSize = 3600
    let level = SrtmLevel.fromInt 1

    let tiles = 
        boundsToTiles tileSize level
            { MinLon = 14.; MinLat = 46.; MaxLon = 16.; MaxLat = 48. }
    let tile = 
        match tiles with 
        | [ tile ] -> tile
        | _ -> invalidOp "bug: there should be only one tile"

    let children = tile |> listChildrenTiles

    let childrenBounds =
        children
        |> Array.fold (fun mbr tile -> 
            tile |> tileLonLatBounds tileSize |> mergeLonLatBounds mbr)
            { MinLon = Double.MaxValue; MinLat = Double.MaxValue; 
                MaxLon = Double.MinValue; MaxLat = Double.MinValue }
    
    test <@ childrenBounds =
                { MinLon = 13.; MinLat = 45.; MaxLon = 17.; MaxLat = 49. }
            @>

let cacheDir = "somecache"

let tileHeights = 
    HeightsArray(1, 2, 3, 4, HeightsArrayInitializer1D (fun _ -> DemHeightNone))

let decodePng: SrtmPngTileReader = fun _ _ -> Ok tileHeights

// fails only on certain tiles
let decodePngWithFailure: SrtmPngTileReader = 
    fun tileId _ ->
        match tileId.TileX = 3 with
        | false -> Ok tileHeights
        | true -> Error "some error"

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

[<Fact>]
let ``Creates the parent tile heights array by downsampling children heights``() =
    let tileSize = 10
    let tile = srtmTileId 2 4 4
    let bufferAroundTile = 1

    let (minChildX, minChildY) = 
        srtmTileId 1 8 8  |> newTileCellMinCoords tileSize
    let (maxChildX, maxChildY) = 
        srtmTileId 1 10 10 |> newTileCellMinCoords tileSize
    let childrenHeightsArray = 
        HeightsArray(
            minChildX - bufferAroundTile,
            minChildY - bufferAroundTile, 
            maxChildX - minChildX + bufferAroundTile * 2,
            maxChildY - minChildY + bufferAroundTile * 2,
            HeightsArrayInitializer1D (fun _ -> DemHeight 100))
    
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