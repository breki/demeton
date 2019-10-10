module Test.Shaders.Hillshading.``Calculating aspect and slope``

open Demeton.Shaders.Terrain
open Demeton.Geometry.Common
open Demeton.Geometry.D3

open System

open Xunit
open FsCheck
open PropertiesHelp
open Swensen.Unquote
open TestHelp

type SlopeCalculator = HeightsWindow -> float -> float -> float option

let hDist = 100.
let vDist = 150.

let referenceSlopeCalculator 
    (heightsWindow: HeightsWindow) horizontalSize verticalSize = 
    let triangleNormalToSlopeAndOrientation normal =
        let normalXYLen = Math.Sqrt(normal.X * normal.X + normal.Y * normal.Y)

        let slope = Math.Atan2(normalXYLen, normal.Z)
        let orientationUnadjusted = Math.Atan2(normal.X, normal.Z)

        let orientation = orientationUnadjusted

        (slope, orientation)

    match someHeightsAreMissing heightsWindow with
    | true -> None
    | false ->
        let points = [|
            { X = 0.; Y = 0.; Z = Option.get heightsWindow.[0] }
            { X = horizontalSize; Y = 0.; Z = Option.get heightsWindow.[1] }
            { X = horizontalSize; Y = verticalSize; 
                Z = Option.get heightsWindow.[2] }
            { X = 0.; Y = verticalSize; Z = Option.get heightsWindow.[3] }
        |]

        let triangle1Normal = triangleNormal points.[0] points.[1] points.[2]
        let triangle2Normal = triangleNormal points.[2] points.[3] points.[0] 
        let triangle3Normal = triangleNormal points.[0] points.[1] points.[3]
        let triangle4Normal = triangleNormal points.[2] points.[3] points.[1]

        let (slope1, orientation1) = 
            triangleNormalToSlopeAndOrientation triangle1Normal
        let (slope2, orientation2) = 
            triangleNormalToSlopeAndOrientation triangle2Normal
        let (slope3, orientation3) = 
            triangleNormalToSlopeAndOrientation triangle3Normal
        let (slope4, orientation4) = 
            triangleNormalToSlopeAndOrientation triangle4Normal

        let slope = (slope1 + slope2 + slope3 + slope4) / 4.
        let orientation = 
            (orientation1 + orientation2 + orientation3 + orientation4) / 4.

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

    let isApproxEqual a b =
        (Option.isNone a && Option.isNone a = Option.isNone b) 
            || (Option.get a =~= Option.get b)

    let rotate (heightsWindow: HeightsWindow)  =
        [| heightsWindow.[3]; heightsWindow.[0]; 
            heightsWindow.[1]; heightsWindow.[2] |]

    let originalValue = slope heightsWindow hDist vDist

    let heights90 = rotate heightsWindow
    let heights180 = rotate heights90
    let heights270 = rotate heights180

    let rotated90Value = slope heights90 vDist hDist 
    let rotated180Value = slope heights180 hDist vDist
    let rotated270Value = slope heights270 vDist hDist 
    
    let is90Sym = isApproxEqual originalValue rotated90Value
    let is180Sym = isApproxEqual originalValue rotated180Value
    let is270Sym = isApproxEqual originalValue rotated270Value
    (is90Sym && is180Sym && is270Sym)
        |@ sprintf 
            "is not symmetric: original:%A, 90:%A, 180:%A, 270:%A" 
            originalValue
            rotated90Value
            rotated180Value
            rotated270Value

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

[<Theory>]
[<InlineData(0., 0., 100., 100., 150., 100., 45.)>]
[<InlineData(100., 0., 0., 100., 100., 150., 45.)>]
[<InlineData(100., 100., 0., 0., 150., 100., 45.)>]
[<InlineData(0., 100., 100., 0., 100., 150., 45.)>]
let ``Some control values`` h1 h2 h3 h4 hDist vDist expectedSlope =
    let heights = [| Some h1; Some h2; Some h3; Some h4; |]
    test <@ referenceSlopeCalculator heights hDist vDist 
        = Some (degToRad expectedSlope) @>

[<Fact>]
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


