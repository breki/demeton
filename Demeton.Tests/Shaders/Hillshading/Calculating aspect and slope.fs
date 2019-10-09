module Test.Shaders.Hillshading.``Calculating aspect and slope``

open Demeton.Shaders.Terrain
open Demeton.Geometry.Common
open Demeton.Geometry.D3

open System
open System.Diagnostics

open Xunit
open FsCheck
open PropertiesHelp
open Swensen.Unquote

type SlopeCalculator = HeightsWindow -> float -> float -> float option

let hDist = 100.
let vDist = 150.

let referenceSlopeCalculator 
    (heightsWindow: HeightsWindow) horizontalSize verticalSize = 
    let triangleNormalToSlopeAndOrientation normal =
        let slopeUnadjusted = Math.Atan2(normal.Y, normal.Z)
        let orientationUnadjusted = Math.Atan2(normal.X, normal.Z)

        let slope = Math.Abs(slopeUnadjusted)
        let orientation = orientationUnadjusted

        if normal.Z > 0. then Debugger.Break()
        if slope <= Math.PI / 2. then Debugger.Break()

        (slope, orientation)

    match someHeightsAreMissing heightsWindow with
    | true -> None
    | false ->
        let points = [|
            { X = 0.; Y = 0.; Z = Option.get heightsWindow.[0] }
            { X = horizontalSize; Y = 0.; Z = Option.get heightsWindow.[1] }
            { X = 0.; Y = verticalSize; Z = Option.get heightsWindow.[2] }
            { X = horizontalSize; Y = verticalSize; 
                Z = Option.get heightsWindow.[3] }
        |]

        let triangle1Normal = triangleNormal points.[0] points.[1] points.[2]
        let triangle2Normal = triangleNormal points.[1] points.[3] points.[2] 
        let (slope1, orientation1) = 
            triangleNormalToSlopeAndOrientation triangle1Normal
        let (slope2, orientation2) = 
            triangleNormalToSlopeAndOrientation triangle2Normal

        let slope = (slope1 + slope2) / 2.
        let orientation = (orientation1 + orientation2) / 2.

        Some slope

let slope2 (heightsWindow: HeightsWindow) horizontalSize verticalSize = 
        invalidOp "todo"

let ``is value from 0 to 90 degrees or None if some heights are missing`` 
    heightsWindow (slope: SlopeCalculator) = 
    let slopeValueMaybe = slope heightsWindow hDist vDist
    let heightsAreMissing = someHeightsAreMissing heightsWindow

    match slopeValueMaybe with
    | None -> 
        heightsAreMissing |> Prop.classify true "heights are missing"
        |@ sprintf "slope should be None"
    | Some slopeValue -> 
        let slopeInDegress = radToDeg slopeValue
        (not heightsAreMissing && slopeInDegress >= 0. 
            && slopeInDegress <= 90.)
        |> Prop.classify true "all heights are specified"
        |@ sprintf 
            "slope should be between 0 and 90 degrees, actually is %A" 
            slopeInDegress

let ``is 90-degrees symmetric`` 
    (heightsWindow: HeightsWindow)
    (slope: SlopeCalculator) =

    let rotate (heightsWindow: HeightsWindow)  =
        [| 
            heightsWindow.[2]; heightsWindow.[0]; 
            heightsWindow.[3]; heightsWindow.[1] |]

    let originalValue = slope heightsWindow hDist vDist
    let rotated90Value = slope (rotate heightsWindow) vDist hDist 
    let rotated180Value = 
        slope (rotate heightsWindow |> rotate) hDist vDist
    let rotated270Value = 
        slope (rotate heightsWindow |> rotate |> rotate) vDist hDist 
    
    let is90Sym = originalValue = rotated90Value
    let is180Sym = originalValue = rotated180Value
    let is270Sym = originalValue = rotated270Value
    (is90Sym && is180Sym && is270Sym)
        |@ sprintf 
            "is not symmetric: original:%f, 90:%f, 180:%f, 270:%f" 
            (radToDeg (Option.get originalValue))
            (radToDeg (Option.get rotated90Value))
            (radToDeg (Option.get rotated180Value))
            (radToDeg (Option.get rotated270Value))

let ``Calculates the same value as the reference implementation``
    (heightsWindow: HeightsWindow)
    (slope: SlopeCalculator) =
    slope heightsWindow hDist vDist = 
        referenceSlopeCalculator heightsWindow hDist vDist

let specs (slope: SlopeCalculator) x = 
    (``is value from 0 to 90 degrees or None if some heights are missing`` x slope)
    .&. (``is 90-degrees symmetric`` x slope)
    .&. (``Calculates the same value as the reference implementation`` x slope)

[<Fact(Skip ="todo next: remove if we implement the alternative function")>]
let ``Calculating slope using method 1``() =
    let genHeight = floatInRange -100 500
    let genHeightMaybe = genHeight |> optionOfWithFrequency 1
    let genHeightsWindow = Gen.arrayOfLength 4 genHeightMaybe

    genHeightsWindow |> Arb.fromGen 
    |> Prop.forAll <| specs slope
    |> Check.VerboseThrowOnFailure

[<Fact>]
let ``Some control value``() =
    let heights = [| Some 0.; Some 0.; Some 100.; Some 100.; |]
    test <@ slope heights 150. 100. = Some 45. @>

[<Fact(Skip="todo working on this one")>]
let ``Calculating slope using reference implementation``() =
    let genHeight = floatInRange -100 500
    let genHeightMaybe = genHeight |> optionOfWithFrequency 1
    let genHeightsWindow = Gen.arrayOfLength 4 genHeightMaybe

    genHeightsWindow |> Arb.fromGen 
    |> Prop.forAll <| specs referenceSlopeCalculator
    |> Check.VerboseThrowOnFailure

[<Fact(Skip ="todo next: implement once we implement the reference calculator")>]
let ``Calculating slope using method 2``() =
    let genHeight = floatInRange -100 500
    let genHeightMaybe = genHeight |> optionOfWithFrequency 1
    let genHeightsWindow = Gen.arrayOfLength 4 genHeightMaybe

    genHeightsWindow |> Arb.fromGen 
    |> Prop.forAll <| specs slope2
    |> Check.QuickThrowOnFailure


//[<Fact>]
//let ``Computing normals``() =
//    let cellSize = 100.
//    let heightDiff = -1000.

//    let A = { X = 0.; Y = 0.; Z = 0. }
//    let B = { X = cellSize; Y = cellSize; Z = heightDiff }
//    let C = { X = 0.; Y = cellSize; Z = heightDiff }

//    let normal = triangleNormal A B C
//    let (normal, azimuth, elevation) = triangleNormal A B C

//    printfn "normal=%A, azimuth=%f, elevation=%f" 
//        normal (radToDeg azimuth) (radToDeg elevation)


