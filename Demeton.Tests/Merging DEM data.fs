module Demeton.Tests.``Merging DEM data``

open Demeton
open Demeton.DemTypes

open FsUnit
open Xunit
open Swensen.Unquote

let someCells _ = None

let heightCellsInitializer 
    (cellsToFill: HeightCell list) (cellCoords: GlobalCellCoords) =
    cellsToFill |> List.tryFind (
        fun cellToFill -> cellToFill.Coords = cellCoords)
    |> function
    | Some cell -> cell.Height
    | _ -> None

[<Fact>]
let ``Merging empty DEM data array results in None``() =
    Dem.merge ([]) |> should equal None

[<Fact>]
let ``Merging single DEM data array results in the same array``() =
    let array = HeightArray({ X = 10; Y = 20}, 15, 25, someCells)
    test <@ Dem.merge ([ array ]) = Some array @>

[<Fact>]
let ``Merging two adjacent DEM data arrays results in a merged array``() =
    let cells1 = [
        { Coords = { X = 11; Y = 22 }; Height = Some 12 }
    ]
    let cells2 = [
        { Coords = { X = 25; Y = 20 }; Height = Some 20 }
    ]

    let array1 = HeightArray({ X = 10; Y = 20}, 15, 25, heightCellsInitializer cells1)
    let array2 = HeightArray({ X = 25; Y = 20}, 15, 25, heightCellsInitializer cells2)
    let mergedMaybe = Dem.merge([ array1; array2 ])

    test <@ (mergedMaybe |> Option.isSome) = true @>

    let merged = mergedMaybe.Value

    test <@ merged.MinCoords.X = 10 @>
    test <@ merged.MinCoords.Y = 20 @>
    test <@ merged.Width = 30 @>
    test <@ merged.Height = 25 @>
    test <@ merged.heightAt { X = 11; Y = 22} = Some 12 @>
    test <@ merged.heightAt { X = 25; Y = 20} = Some 20 @>

[<Fact>]
let ``Merging several DEM data arrays results in a merged array``() =
    let array1 = HeightArray({ X = 10; Y = 20}, 15, 25, someCells)
    let array2 = HeightArray({ X = 25; Y = 20}, 15, 25, someCells)
    let array3 = HeightArray({ X = 100; Y = 0}, 15, 25, someCells)
    let mergedMaybe = Dem.merge([ array1; array2; array3 ])

    test <@ (mergedMaybe |> Option.isSome) = true @>

    let merged = mergedMaybe.Value

    test <@ merged.MinCoords.X = 10 @>
    test <@ merged.MinCoords.Y = 0 @>
    test <@ merged.Width = 105 @>
    test <@ merged.Height = 45 @>
