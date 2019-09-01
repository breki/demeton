module Demeton.Tests.``Merging DEM data``

open Demeton
open Demeton.DemTypes

open FsUnit
open Xunit
open Swensen.Unquote

let someCells _ _ = None

let heightCellsInitializer (cells: HeightCell list) x y =
    cells |> List.tryFind (fun c -> c.X = x && c.Y = y)
    |> function
    | Some cell -> cell.Height
    | _ -> None

[<Fact>]
let ``Merging empty DEM data array results in None``() =
    Dem.merge ([]) |> should equal None

[<Fact>]
let ``Merging single DEM data array results in the same array``() =
    let array = HeightArray(10, 20, 15, 25, someCells)
    test <@ Dem.merge ([ array ]) = Some array @>

[<Fact>]
let ``Merging two adjacent DEM data arrays results in a merged array``() =
    let cells1 = [
        { X = 11; Y = 22; Height = Some 12 }
    ]
    let cells2 = [
        { X = 25; Y = 20; Height = Some 20 }
    ]

    let array1 = HeightArray(10, 20, 15, 25, heightCellsInitializer cells1)
    let array2 = HeightArray(25, 20, 15, 25, heightCellsInitializer cells2)
    let merged = Dem.merge([ array1; array2 ])

    test <@ (merged |> Option.isSome) = true @>
    test <@ merged.Value.MinX = 10 @>
    test <@ merged.Value.MinY = 20 @>
    test <@ merged.Value.Width = 30 @>
    test <@ merged.Value.Height = 25 @>
    test <@ merged.Value.heightAt 11 22 = Some 12 @>
    test <@ merged.Value.heightAt 25 20 = Some 20 @>

[<Fact>]
let ``Merging several DEM data arrays results in a merged array``() =
    let array1 = HeightArray(10, 20, 15, 25, someCells)
    let array2 = HeightArray(25, 20, 15, 25, someCells)
    let array3 = HeightArray(100, 0, 15, 25, someCells)
    let merged = Dem.merge([ array1; array2; array3 ])

    test <@ (merged |> Option.isSome) = true @>
    test <@ merged.Value.MinX = 10 @>
    test <@ merged.Value.MinY = 0 @>
    test <@ merged.Value.Width = 105 @>
    test <@ merged.Value.Height = 45 @>
