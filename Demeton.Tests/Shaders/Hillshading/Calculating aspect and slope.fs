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

let hDist = 100.
let vDist = 150.

let isSlopeApproxEqual 
    (a: SlopeAndOrientation option) (b: SlopeAndOrientation option) =
    (Option.isNone a && Option.isNone a = Option.isNone b) 
        || (fst (Option.get a) =~= fst (Option.get b))

/// <summary>
/// Reference implementation of the calculator of slope and orientation that is
/// used as a test oracle for the actual (faster) production implementation.
/// </summary>
let referenceSlopeAndOrientationCalculator: SlopeAndOrientationCalculator 
    = fun (heightsWindow: HeightsWindow) horizontalSize verticalSize ->
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

        Some (slope, orientation)

[<Theory>]
[<InlineData(0., 0., 100., 100., 150., 100., 45.)>]
[<InlineData(100., 0., 0., 100., 100., 150., 45.)>]
[<InlineData(100., 100., 0., 0., 150., 100., 45.)>]
[<InlineData(0., 100., 100., 0., 100., 150., 45.)>]
[<InlineData(0., 0., 0., 0., 100., 150., 0.)>]
let ``Some control values for the reference implementation`` 
    h1 h2 h3 h4 hDist vDist expectedSlope =
    let heights = [| Some h1; Some h2; Some h3; Some h4; |]

    let slopeMaybe = 
        referenceSlopeAndOrientationCalculator heights hDist vDist

    test <@ Option.isSome slopeMaybe @>
    test <@ fst (Option.get slopeMaybe) = degToRad expectedSlope @>


let ``is value from 0 to 90 degrees or None if some heights are missing`` 
    heightsWindow (slope: SlopeAndOrientationCalculator) = 
    let slopeValueMaybe = slope heightsWindow hDist vDist
    let heightsAreMissing = someHeightsAreMissing heightsWindow

    match slopeValueMaybe with
    | None -> 
        heightsAreMissing |> Prop.classify true "heights are missing"
        |@ sprintf "slope should be None"
    | Some (slopeValue, _) -> 
        let slopeInDegress = radToDeg slopeValue
        (not heightsAreMissing && slopeInDegress >= 0. 
            && slopeInDegress <= 90.)
        |> Prop.classify true "all heights are specified"
        |@ sprintf 
            "slope should be between 0 and 90 degrees, actually is %A" 
            slopeInDegress

let ``is 90-degrees symmetric`` 
    (heightsWindow: HeightsWindow)
    (calculator: SlopeAndOrientationCalculator) =

    let rotate (heightsWindow: HeightsWindow)  =
        [| heightsWindow.[3]; heightsWindow.[0]; 
            heightsWindow.[1]; heightsWindow.[2] |]

    let originalValues = calculator heightsWindow hDist vDist

    let heights90 = rotate heightsWindow
    let heights180 = rotate heights90
    let heights270 = rotate heights180

    let rotated90Values = calculator heights90 vDist hDist 
    let rotated180Values = calculator heights180 hDist vDist
    let rotated270Values = calculator heights270 vDist hDist 
    
    let is90Sym = isSlopeApproxEqual originalValues rotated90Values
    let is180Sym = isSlopeApproxEqual originalValues rotated180Values
    let is270Sym = isSlopeApproxEqual originalValues rotated270Values
    (is90Sym && is180Sym && is270Sym)
        |@ sprintf 
            "is not symmetric: original:%A, 90:%A, 180:%A, 270:%A" 
            originalValues
            rotated90Values
            rotated180Values
            rotated270Values

let ``calculates the same value as the reference implementation``
    (heightsWindow: HeightsWindow)
    (calculator: SlopeAndOrientationCalculator) =
    isSlopeApproxEqual
        (calculator heightsWindow hDist vDist)
        (referenceSlopeAndOrientationCalculator heightsWindow hDist vDist)
        |@ sprintf "is not the same as reference implementation"

let specs (calculator: SlopeAndOrientationCalculator) x = 
    (``is value from 0 to 90 degrees or None if some heights are missing`` x calculator)
    .&. (``is 90-degrees symmetric`` x calculator)
    .&. (``calculates the same value as the reference implementation`` x calculator)

[<Fact>]
let ``The reference slope implementation adheres to all the properties``() =
    let genHeight = floatInRange -100 500
    let genHeightMaybe = genHeight |> optionOfWithFrequency 1
    let genHeightsWindow = Gen.arrayOfLength 4 genHeightMaybe

    genHeightsWindow |> Arb.fromGen 
    |> Prop.forAll 
    <| specs referenceSlopeAndOrientationCalculator
    |> Check.QuickThrowOnFailure

[<Fact>]
let ``Production slope implementation adheres to all the properties``() =
    let genHeight = floatInRange -100 500
    let genHeightMaybe = genHeight |> optionOfWithFrequency 1
    let genHeightsWindow = Gen.arrayOfLength 4 genHeightMaybe

    genHeightsWindow |> Arb.fromGen 
    |> Prop.forAll 
    <| specs calculateSlopeAndOrientation
    |> Check.QuickThrowOnFailure
