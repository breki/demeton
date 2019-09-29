module Dem.``Working with HeightsArrays``

open Demeton.DemTypes

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open TestHelp

type NeighboringHeights = DemHeight option[] * int * int
type NeighboringHeightsGenerator =
    static member NeighboringHeights() =
        let heights =
            Arb.generate<DemHeight>
            |> Gen.optionOf |> Gen.arrayOfLength 4

        let x = Gen.choose (0, 1000)
        let y = Gen.choose (0, 1000)

        Gen.zip3 heights x y
        |> Gen.map (fun (h, x, y) -> NeighboringHeights(h, x, y))
        |> Arb.fromGen

type NeighboringHeightsPropertyAttribute() = 
    inherit PropertyAttribute
        (Arbitrary = [| typeof<NeighboringHeightsGenerator> |],
        QuietOnSuccess = true)

let interpolateHeight h1 h2 h3 h4 (dx: float) (dy: float) =
    if h1 = DemHeightNone || h2 = DemHeightNone
        || h3 = DemHeightNone || h4 = DemHeightNone then
        None
    else
        let hh1 = float (h2 - h1) * dx + float h1
        let hh2 = float (h4 - h3) * dx + float h3
        let height = float (hh2 - hh1) * dy + hh1
        Some height

[<NeighboringHeightsProperty>]
let ``Interpolating heights from neighboring cells`` 
    (heights: NeighboringHeights) =
    
    let fromOptionalHeight (optionalHeight: DemHeight option) =
        match optionalHeight with
        | Some height -> height
        | None -> DemHeightNone

    let (optionalHeights, xint, yint) = heights

    let nonoptionalHeights = optionalHeights |> Array.map fromOptionalHeight
    let anyMissingHeights = optionalHeights |> Array.exists Option.isNone

    let x = float xint / 1000.
    let y = float yint / 1000.

    let h1 = nonoptionalHeights.[0]
    let h2 = nonoptionalHeights.[1]
    let h3 = nonoptionalHeights.[2]
    let h4 = nonoptionalHeights.[3]

    let heightCalcOption = interpolateHeight h1 h2 h3 h4 x y

    match anyMissingHeights with
    | true -> Option.isNone heightCalcOption 
    | false ->
        match heightCalcOption with
        | None -> false
        | Some heightCalc -> 
            let minHeight = (min h1 h2) |> min h3 |> min h4 |> float
            let maxHeight = (max h1 h2) |> max h3 |> max h4 |> float

            printfn 
                "heightCalc = %f, min = %f, max = %f" 
                heightCalc minHeight maxHeight
            heightCalc >= minHeight && heightCalc <= maxHeight


[<Fact>]
let ``Interpolates height correctly``() =
    let heightCalcOption = 
        interpolateHeight 11s -4s -5s 4s (88. / 1000.) (786. / 1000.)
    test <@ Option.isSome heightCalcOption @>
    test <@ Option.get heightCalcOption |> isApproxEqualTo -1.235968 6 @>
