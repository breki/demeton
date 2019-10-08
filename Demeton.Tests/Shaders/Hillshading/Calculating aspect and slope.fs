module Test.Shaders.Hillshading.``Calculating aspect and slope``

open Demeton.Shaders.Terrain

open Xunit
open FsCheck
open PropertiesHelp
open Swensen.Unquote

[<Fact>]
let ``Calculating slope``() =
    let ``is value from 0 to 90 degrees or None if some heights are missing`` 
        heightsWindow = 
        let slopeValueMaybe = slope heightsWindow 100. 150.
        let heightsAreMissing = someHeightsAreMissing heightsWindow

        match slopeValueMaybe with
        | None -> heightsAreMissing
        | Some slopeValue -> 
            (not heightsAreMissing && slopeValue >= 0. && slopeValue <= 90.)

    let ``is 90-degrees simetric`` (heightsWindow: HeightsWindow) =
        let hDist = 100.
        let vDist = 150.

        let rotate (heightsWindow: HeightsWindow)  =
            [| 
                heightsWindow.[2]; heightsWindow.[0]; 
                heightsWindow.[3]; heightsWindow.[1] |]

        let originalSlopeValue = slope heightsWindow hDist vDist
        let rotated90Value = slope (rotate heightsWindow) vDist hDist 
        let rotated180Value = 
            slope (rotate heightsWindow |> rotate) hDist vDist
        let rotated270Value = 
            slope (rotate heightsWindow |> rotate |> rotate) vDist hDist 
        originalSlopeValue = rotated90Value
            && originalSlopeValue = rotated180Value
            && originalSlopeValue = rotated270Value

    let specs x = 
        (``is value from 0 to 90 degrees or None if some heights are missing`` x)
        .&. (``is 90-degrees simetric`` x)

    let genHeight = floatInRange -100 500
    let genHeightMaybe = genHeight |> optionOfWithFrequency 1
    let genHeightsWindow = Gen.arrayOfLength 4 genHeightMaybe

    genHeightsWindow |> Arb.fromGen 
    |> Prop.forAll <| specs
    |> Check.VerboseThrowOnFailure

[<Fact>]
let ``Some control value``() =
    let heights = [| Some 0.; Some 0.; Some 100.; Some 100.; |]
    test <@ slope heights 150. 100. = Some 45. @>