module Tests.Dem.``Merging height arrays``

open Demeton.Dem.Types

open FsUnit
open Raster
open Xunit
open Swensen.Unquote

let someCells = HeightsArrayInitializer1D(fun _ -> DemHeightNone)

let heightCellsInitializer (cellsToFill: HeightCell list) =
    fun cellCoords ->
        cellsToFill
        |> List.tryFind (fun cellToFill -> cellToFill.Coords = cellCoords)
        |> function
            | Some cell -> cell.Height
            | _ -> DemHeightNone

[<Fact>]
let ``Merging empty DEM data array results in None`` () =
    Demeton.Dem.Funcs.merge Rect.Empty [] |> should equal None

[<Fact>]
let ``Merging several DEM data arrays results in a merged array`` () =
    let cells1 = [ { Coords = (11, 22); Height = 12s } ]
    let cells2 = [ { Coords = (25, 20); Height = 20s } ]

    let array1 =
        HeightsArray(
            10,
            20,
            15,
            25,
            HeightsArrayInitializer2D(fun x -> heightCellsInitializer cells1 x)
        )

    let array2 =
        HeightsArray(
            25,
            20,
            15,
            25,
            HeightsArrayInitializer2D(fun x -> heightCellsInitializer cells2 x)
        )

    let array3 = HeightsArray(100, 0, 15, 25, someCells)
    let arrays = [ array1; array2; array3 ]
    let mbr = Demeton.Dem.Funcs.mbrOfHeightsArrays arrays
    let mergedMaybe = Demeton.Dem.Funcs.merge mbr arrays

    test <@ (mergedMaybe |> Option.isSome) @>

    let merged = mergedMaybe.Value

    test <@ merged.MinX = 10 @>
    test <@ merged.MinY = 0 @>
    test <@ merged.Width = 105 @>
    test <@ merged.Height = 45 @>
    test <@ merged.heightAt (11, 22) = 12s @>
    test <@ merged.heightAt (25, 20) = 20s @>
