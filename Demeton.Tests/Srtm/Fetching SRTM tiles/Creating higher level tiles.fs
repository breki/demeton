module Tests.Srtm.``Fetching SRTM tiles``.``Creating higher level tiles``

open Demeton.Srtm.Funcs
open Demeton.Srtm.Types
open Demeton.Geometry.Common
open Demeton.Srtm.Downsampling
open Demeton.DemTypes

open System

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Correctly calculates the list of needed children for level 1 for SomeFutureMethod (case 1)``() =
    let expectedChildren =
        [| 
            (-1, -1); (0, -1); (1, -1); (2, -1); 
            (-1, 0); (0, 0); (1, 0); (2, 0); 
            (-1, 1); (0, 1); (1, 1); (2, 1); 
            (-1, 2); (0, 2); (1, 2); (2, 2);
        |]
        |> Array.map (fun (x, y) -> srtmTileId 0 x y)
    
    test <@ srtmTileId 1 0 0 
        |> childrenTilesNeededForDownsampling 
            DownsamplingMethod.SomeFutureMethod
            = expectedChildren @>

let private tileFromBounds tileSize level minLon minLat maxLon maxLat =
    let tiles = 
        boundsToTiles tileSize level
            { MinLon = minLon |> float; MinLat = minLat |> float; 
            MaxLon = maxLon |> float; MaxLat = maxLat |> float }
    match tiles with 
    | [ tile ] -> tile
    | _ -> invalidOp "bug: there should be only one tile"

[<Fact>]
let ``Correctly calculates the list of needed children for level 1 for SomeFutureMethod (case 2)``() =
    let tileSize = 3600
    let level = SrtmLevel.fromInt 1

    let tile = tileFromBounds tileSize level 14 46 16 48

    let children = 
        tile 
        |> childrenTilesNeededForDownsampling 
            DownsamplingMethod.SomeFutureMethod

    let childrenBounds =
        children
        |> Array.fold (fun mbr tile -> 
            tile |> tileLonLatBounds tileSize |> mergeLonLatBounds mbr)
            { MinLon = Double.MaxValue; MinLat = Double.MaxValue; 
                MaxLon = Double.MinValue; MaxLat = Double.MinValue }
    
    test <@ childrenBounds =
                { MinLon = 13.; MinLat = 45.; MaxLon = 17.; MaxLat = 49. }
            @>

[<Fact>]
let ``Correctly calculates the list of needed children for level 1 for Average method``() =
    let tileSize = 3600
    let level = SrtmLevel.fromInt 1

    let tile = tileFromBounds tileSize level 14 46 16 48

    let children = 
        tile 
        |> childrenTilesNeededForDownsampling DownsamplingMethod.Average

    let childrenBounds =
        children
        |> Array.fold (fun mbr tile -> 
            tile |> tileLonLatBounds tileSize |> mergeLonLatBounds mbr)
            { MinLon = Double.MaxValue; MinLat = Double.MaxValue; 
                MaxLon = Double.MinValue; MaxLat = Double.MinValue }
    
    test <@ childrenBounds =
                { MinLon = 14.; MinLat = 46.; MaxLon = 16.; MaxLat = 48. }
            @>

[<Fact>]
let ``Creates the parent tile heights array by downsampling children heights (SomeFutureMethod)``() =
    let tileSize = 10
    let tile = srtmTileId 2 4 4
    let bufferAroundTile = 1

    let (minChildX, minChildY) = 
        srtmTileId 1 8 8  |> tileMinCell tileSize
    let (maxChildX, maxChildY) = 
        srtmTileId 1 10 10 |> tileMinCell tileSize
    let childrenHeightsArray = 
        HeightsArray(
            minChildX - bufferAroundTile,
            minChildY - bufferAroundTile, 
            maxChildX - minChildX + bufferAroundTile * 2,
            maxChildY - minChildY + bufferAroundTile * 2,
            HeightsArrayInitializer1D (fun _ -> DemHeight 100))
    
    test <@ childrenHeightsArray.Width = (tileSize + bufferAroundTile) * 2 @>
    test <@ childrenHeightsArray.Height = (tileSize + bufferAroundTile) * 2 @>
    
    let parentHeights = 
        downsampleTileHeightsArray
            DownsamplingMethod.SomeFutureMethod
            tileSize tile childrenHeightsArray

    let (expectedParentTileX, expectedParentTileY)
        = tile |> tileMinCell tileSize
    test <@ parentHeights.MinX = expectedParentTileX @>
    test <@ parentHeights.MinY = expectedParentTileY @>
    test <@ parentHeights.Width = tileSize @>
    test <@ parentHeights.Height = tileSize @>
    
[<Fact>]
let ``Average calculates the average of the 2x2 grid heights``() =
    let heights = 
        [| DemHeight 100s; DemHeight 150s; DemHeight 50s; DemHeight -103s |]
    
    test <@ downsampleAverage heights = 49s @>
    
[<Fact>]
let ``Average ignores missing heights``() =
    let heights = 
        [| DemHeightNone; DemHeight 150s; DemHeightNone; DemHeight -103s |]
    
    test <@ downsampleAverage heights = 24s @>
    
[<Fact>]
let ``If all heights are missing, Average returns DemHeightNone``() =
    let heights = 
        [| DemHeightNone; DemHeightNone; DemHeightNone; DemHeightNone |]
    
    test <@ downsampleAverage heights = DemHeightNone @>

