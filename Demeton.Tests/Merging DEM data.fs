module Demeton.Tests.``Merging DEM data``

open Demeton
open Demeton.DemTypes

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Merging empty DEM data array results in None``() =
    Dem.merge ([]) |> should equal None

[<Fact>]
let ``Merging single DEM data array results in the same array``() =
    let array = DemData(10, 20, 15, 25)
    test <@ Dem.merge ([ array ]) = Some array @>

[<Fact>]
let ``Merging two adjacent DEM data arrays results in a merged array``() =
    let array1 = DemData(10, 20, 15, 25)
    let array2 = DemData(25, 20, 15, 25)
    let merged = Dem.merge([ array1; array2 ])

    test <@ (merged |> Option.isSome) = true @>
    test <@ merged.Value.MinX = 10 @>
    test <@ merged.Value.MinY = 20 @>
    test <@ merged.Value.Width = 30 @>
    test <@ merged.Value.Height = 25 @>


[<Fact>]
let ``Merging several DEM data arrays results in a merged array``() =
    let array1 = DemData(10, 20, 15, 25)
    let array2 = DemData(25, 20, 15, 25)
    let array3 = DemData(100, 0, 15, 25)
    let merged = Dem.merge([ array1; array2; array3 ])

    test <@ (merged |> Option.isSome) = true @>
    test <@ merged.Value.MinX = 10 @>
    test <@ merged.Value.MinY = 0 @>
    test <@ merged.Value.Width = 105 @>
    test <@ merged.Value.Height = 45 @>
