module Tests.Dem.``Working with HeightsArrays``

open Demeton.Dem.Types

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open TestHelp

type NeighboringHeights = DemHeight option[] * int * int

type NeighboringHeightsGenerator =
    static member NeighboringHeights() =
        let heights =
            Arb.generate<DemHeight> |> Gen.optionOf |> Gen.arrayOfLength 4

        let x = Gen.choose (0, 1000)
        let y = Gen.choose (0, 1000)

        Gen.zip3 heights x y
        |> Gen.map (fun (h, x, y) -> NeighboringHeights(h, x, y))
        |> Arb.fromGen

type NeighboringHeightsPropertyAttribute() =
    inherit
        PropertyAttribute(
            Arbitrary = [| typeof<NeighboringHeightsGenerator> |],
            QuietOnSuccess = true
        )

[<NeighboringHeightsProperty>]
let ``Interpolating heights from neighboring cells``
    (heights: NeighboringHeights)
    =

    let fromOptionalHeight (optionalHeight: DemHeight option) =
        match optionalHeight with
        | Some height -> height
        | None -> DemHeightNone

    let optionalHeights, xint, yint = heights

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
            heightCalc >= minHeight && heightCalc <= maxHeight


[<Fact>]
let ``Interpolates height correctly`` () =
    let heightCalcOption =
        interpolateHeight 11s -4s -5s 4s (88. / 1000.) (786. / 1000.)

    test <@ Option.isSome heightCalcOption @>

    test
        <@ Option.get heightCalcOption |> isApproxEqualTo -1.235968 (Decimals 6) @>

[<Fact>]
let ``Interpolates height in the heights array correctly`` () =
    let globalCellX = 100
    let globalCellY = 200

    let heightsArray =
        HeightsArray(
            globalCellX,
            globalCellY,
            10,
            10,
            HeightsArrayInitializer1D(fun _ -> DemHeightNone)
        )

    heightsArray.setHeightAt (globalCellX + 5, globalCellY + 5) 11s
    heightsArray.setHeightAt (globalCellX + 6, globalCellY + 5) -4s
    heightsArray.setHeightAt (globalCellX + 5, globalCellY + 6) -5s
    heightsArray.setHeightAt (globalCellX + 6, globalCellY + 6) 4s

    let heightMaybe =
        heightsArray.interpolateHeightAt (
            float globalCellX + 5.088,
            float globalCellY + 5.786
        )

    test <@ Option.isSome heightMaybe @>
    test <@ Option.get heightMaybe |> isApproxEqualTo -1.235968 (Decimals 6) @>
