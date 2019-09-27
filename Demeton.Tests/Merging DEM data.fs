module Demeton.Tests.``Merging DEM data``

open Demeton
open Demeton.DemTypes

open FsUnit
open Xunit
open Swensen.Unquote

let someCells = HeightsArrayInitializer1D (fun _ -> DemHeightNone)

let heightCellsInitializer (cellsToFill: HeightCell list) =
    fun cellCoords -> 
        cellsToFill |> List.tryFind (
            fun cellToFill -> cellToFill.Coords = cellCoords)
        |> function
        | Some cell -> cell.Height
        | _ -> DemHeightNone

[<Fact>]
let ``Merging empty DEM data array results in None``() =
    Dem.merge ([]) |> should equal None

[<Fact>]
let ``Merging single DEM data array results in the same array``() =
    let array = HeightsArray(10, 20, 15, 25, someCells)
    test <@ Dem.merge ([ array ]) = Some array @>

[<Fact>]
let ``Merging several DEM data arrays results in a merged array``() =
    let cells1 = [
        { Coords = (11, 22); Height = 12s }
    ]
    let cells2 = [
        { Coords = (25, 20); Height = 20s }
    ]

    let array1 = 
        HeightsArray(
            10, 20, 15, 25, HeightsArrayInitializer2D (
                fun x -> heightCellsInitializer cells1 x))
    let array2 = 
        HeightsArray(
            25, 20, 15, 25, HeightsArrayInitializer2D (
                fun x -> heightCellsInitializer cells2 x))
    let array3 = HeightsArray(100, 0, 15, 25, someCells)
    let mergedMaybe = Dem.merge([ array1; array2; array3 ])

    test <@ (mergedMaybe |> Option.isSome) = true @>

    let merged = mergedMaybe.Value

    test <@ merged.MinX = 10 @>
    test <@ merged.MinY = 0 @>
    test <@ merged.Width = 105 @>
    test <@ merged.Height = 45 @>
    test <@ merged.heightAt (11, 22) = 12s @>
    test <@ merged.heightAt (25, 20) = 20s @>
