module Tests.Srtm.``Fetching SRTM tiles``.``Creating higher level tiles``

open Demeton.Geometry.Common
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Srtm.Funcs
open Demeton.Srtm.Downsampling

open System

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Correctly calculates the list of needed children for level 1 for SomeFutureMethod (case 1)``
    ()
    =
    let expectedChildren =
        [| (-1, -1)
           (0, -1)
           (1, -1)
           (2, -1)
           (-1, 0)
           (0, 0)
           (1, 0)
           (2, 0)
           (-1, 1)
           (0, 1)
           (1, 1)
           (2, 1)
           (-1, 2)
           (0, 2)
           (1, 2)
           (2, 2) |]
        |> Array.map (fun (x, y) -> demTileId 0 x y)

    test
        <@
            demTileId 1 0 0
            |> childrenTilesNeededForDownsampling
                DownsamplingMethod.SomeFutureMethod = expectedChildren
        @>

let private tileFromBounds tileSize level minLon minLat maxLon maxLat =
    let tiles =
        boundsToTiles
            tileSize
            level
            { MinLon = minLon |> float
              MinLat = minLat |> float
              MaxLon = maxLon |> float
              MaxLat = maxLat |> float }

    match tiles with
    | [ tile ] -> tile
    | _ -> invalidOp "bug: there should be only one tile"

[<Fact>]
let ``Correctly calculates the list of needed children for level 1 for SomeFutureMethod (case 2)``
    ()
    =
    let tileSize = 3600
    let level = DemLevel.fromInt 1

    let tile = tileFromBounds tileSize level 14 46 16 48

    let children =
        tile
        |> childrenTilesNeededForDownsampling
            DownsamplingMethod.SomeFutureMethod

    let childrenBounds =
        children
        |> Array.fold
            (fun mbr tile ->
                tile |> tileLonLatBounds tileSize |> mergeLonLatBounds mbr)
            { MinLon = Double.MaxValue
              MinLat = Double.MaxValue
              MaxLon = Double.MinValue
              MaxLat = Double.MinValue }

    test
        <@
            childrenBounds = { MinLon = 13.
                               MinLat = 45.
                               MaxLon = 17.
                               MaxLat = 49. }
        @>

[<Fact>]
let ``Correctly calculates the list of needed children for level 1 for Average method``
    ()
    =
    let tileSize = 3600
    let level = DemLevel.fromInt 1

    let tile = tileFromBounds tileSize level 14 46 16 48

    let children =
        tile |> childrenTilesNeededForDownsampling DownsamplingMethod.Average

    let childrenBounds =
        children
        |> Array.fold
            (fun mbr tile ->
                tile |> tileLonLatBounds tileSize |> mergeLonLatBounds mbr)
            { MinLon = Double.MaxValue
              MinLat = Double.MaxValue
              MaxLon = Double.MinValue
              MaxLat = Double.MinValue }

    test
        <@
            childrenBounds = { MinLon = 14.
                               MinLat = 46.
                               MaxLon = 16.
                               MaxLat = 48. }
        @>

[<Fact>]
let ``Average calculates the average of the 2x2 grid heights`` () =
    let height =
        downsampleAverage
            (DemHeight 100s)
            (DemHeight 150s)
            (DemHeight 50s)
            (DemHeight -103s)

    test <@ height = 49s @>

[<Fact>]
let ``Average ignores missing heights`` () =
    let height =
        downsampleAverage
            DemHeightNone
            (DemHeight 150s)
            DemHeightNone
            (DemHeight -103s)


    test <@ height = 24s @>

[<Fact>]
let ``If all heights are missing, Average returns DemHeightNone`` () =
    let height =
        downsampleAverage
            DemHeightNone
            DemHeightNone
            DemHeightNone
            DemHeightNone

    test <@ height = DemHeightNone @>
