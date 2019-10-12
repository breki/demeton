module Test.Shaders.Hillshading.``Calculating aspect and slope``

open Demeton.Shaders.Terrain
open Demeton.Geometry.Common
open Demeton.Geometry.D3

open System

open Xunit
open FsCheck
open PropertiesHelp
open Swensen.Unquote

let hDist = 100.
let vDist = 150.

let areAnglesApproxEqual equalityTolerance a b = 
    differenceBetweenAngles a b (Math.PI*2.) <  equalityTolerance   

let rotateHeights (heightsWindow: HeightsWindow)  =
    [| heightsWindow.[3]; heightsWindow.[0]; 
        heightsWindow.[1]; heightsWindow.[2] |]

let rotateSlopeAndAspect slopeAndAspect rotationAngle =
    match slopeAndAspect with
    | None -> None
    | Some (slope, aspect) ->
        Some (slope, 
            normalizeAngle (aspect + rotationAngle) (Math.PI * 2.))

let slopeAndAspectAreEqual 
    aspectTolerance
    (a: SlopeAndAspect option) 
    (b: SlopeAndAspect option) =

    match (a, b) with
    | (None, None) -> true
    | (_, None) -> false
    | (None, _) -> false
    | (Some (slope1, aspect1), Some (slope2, aspect2)) ->
        match (Double.IsNaN aspect1, Double.IsNaN aspect2) with
        | (true, false) -> false
        | (false, true) -> false
        | (true, true) -> areAnglesApproxEqual 1E-8 slope1 slope2
        | _ -> 
            (areAnglesApproxEqual 1E-8 slope1 slope2) 
            && (areAnglesApproxEqual 
                    aspectTolerance aspect1 aspect2)

/// <summary>
/// Reference implementation of the calculator of slope and aspect that is
/// used as a test oracle for the actual (faster) production implementation.
/// </summary>
let referenceSlopeAndAspectCalculator: SlopeAndAspectCalculator 
    = fun (heightsWindow: HeightsWindow) horizontalSize verticalSize ->
    let triangleNormalToSlopeAndAspect normal =
        let normalXYLen = Math.Sqrt(normal.X * normal.X + normal.Y * normal.Y)

        let slope = Math.Atan2(normalXYLen, normal.Z)
        let aspect = 
            match slope with
            | 0. -> Double.NaN
            | _ -> 
                let aspectNotNormalized = (Math.Atan2(normal.X, -normal.Y))
                let aspectNormalized = 
                    normalizeAngle aspectNotNormalized (Math.PI * 2.)
                aspectNormalized

        (slope, aspect)

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

        let slopesAndAspects = [|
            triangleNormalToSlopeAndAspect triangle1Normal
            triangleNormalToSlopeAndAspect triangle2Normal
            triangleNormalToSlopeAndAspect triangle3Normal
            triangleNormalToSlopeAndAspect triangle4Normal
            |]

        let slopeAverage =
            slopesAndAspects |> Array.averageBy (fun(slope, _) -> slope)

        let aspectsNonNan =
            slopesAndAspects 
            |> Array.filter (
                fun(_, aspect) -> not (Double.IsNaN aspect))
            |> Array.map(fun(_, aspect) -> aspect)

        let aspectAverage =
            normalizeAngle (meanOfAngles 1E-10 aspectsNonNan) (Math.PI*2.)

        Some (slopeAverage, aspectAverage)

[<Theory>]
[<InlineData(0., 0., 100., 100., 150., 100., 45., 0.)>]
[<InlineData(100., 0., 0., 100., 100., 150., 45., 90.)>]
[<InlineData(100., 100., 0., 0., 150., 100., 45., 180.)>]
[<InlineData(0., 100., 100., 0., 100., 150., 45., 270.)>]
[<InlineData(0., 0., 0., 0., 100., 100., 0., Double.NaN)>]
[<InlineData(0., 0., 0., 100., 100., 100., 36.183902577601, 45.)>]
[<InlineData(111.33, 197.02, 162.14, 128.89, 100., 150., 
    30.9898012775403, 264.355332538424)>]
[<InlineData(172.49, 137.61, 224.93, 190.05, 100., 150., 
    27.1808453194676, 0.)>]
let ``Some control values for the reference implementation`` 
    h1 h2 h3 h4 hDist vDist expectedSlope expectedAspect =
    let heights = [| Some h1; Some h2; Some h3; Some h4; |]

    let expectedValues = 
        Some (degToRad expectedSlope, degToRad expectedAspect)
    let expectedRotatedValues = 
        rotateSlopeAndAspect expectedValues (Math.PI / 2.)

    let actualValues = 
        referenceSlopeAndAspectCalculator heights hDist vDist
    let actualRotatedValues = 
        referenceSlopeAndAspectCalculator 
            (heights |> rotateHeights) vDist hDist
    
    test <@ slopeAndAspectAreEqual 0.01 actualValues expectedValues @>
    test <@ slopeAndAspectAreEqual 
        0.01 actualRotatedValues expectedRotatedValues @>


let ``slope is value from 0 to 90 degrees and aspect from 0 to 360 degrees or None if some heights are missing`` 
    heightsWindow (slope: SlopeAndAspectCalculator) = 
    let slopeValueMaybe = slope heightsWindow hDist vDist
    let heightsAreMissing = someHeightsAreMissing heightsWindow

    match slopeValueMaybe with
    | None -> 
        heightsAreMissing |> Prop.classify true "heights are missing"
        |@ sprintf "slope should be None"
    | Some (slope, aspect) -> 
        let slopeInDegress = radToDeg slope
        let aspectInDegress = radToDeg aspect

        (not heightsAreMissing && slopeInDegress >= 0. 
            && slopeInDegress < 90.
            && aspectInDegress >= 0.
            && aspectInDegress < 360.)
        |> Prop.classify true "all heights are specified"
        |@ sprintf 
            "slope should be between 0 and 90 degrees, aspect should be between 0 and 360 degrees, actually is %A" 
            (slopeInDegress, aspectInDegress)

let ``is 90-degrees symmetric`` 
    (heightsWindow: HeightsWindow)
    (calculator: SlopeAndAspectCalculator) =

    let originalValues = calculator heightsWindow hDist vDist

    let expected90 = rotateSlopeAndAspect originalValues (degToRad 90.)
    let expected180 = rotateSlopeAndAspect originalValues (degToRad 180.)
    let expected270 = rotateSlopeAndAspect originalValues (degToRad 270.)

    let heights90 = rotateHeights heightsWindow
    let heights180 = rotateHeights heights90
    let heights270 = rotateHeights heights180

    let actual90 = calculator heights90 vDist hDist 
    let actual180 = calculator heights180 hDist vDist
    let actual270 = calculator heights270 vDist hDist 

    let is90Sym = slopeAndAspectAreEqual 0.01 actual90 expected90 
    let is180Sym = slopeAndAspectAreEqual 0.01 actual180 expected180 
    let is270Sym = slopeAndAspectAreEqual 0.01 actual270 expected270

    (is90Sym && is180Sym && is270Sym)
        |@ sprintf 
            "is not symmetric: original:%A, expected 90:%A, actual 90:%A, expected 180:%A, actual 180:%A, expected 270:%A, actual 270:%A" 
            originalValues
            expected90
            actual90
            expected180
            actual180
            expected270
            actual270

let ``calculates the same value as the reference implementation``
    (heightsWindow: HeightsWindow)
    (calculator: SlopeAndAspectCalculator) =
    let referenceValue = 
        referenceSlopeAndAspectCalculator heightsWindow hDist vDist
    let productionValue = calculator heightsWindow hDist vDist
    slopeAndAspectAreEqual
        // We use a tolerance of 1 to compare reference and production 
        // aspects because they are calculated differently (the production
        // one should actually be more precise).
        1.
        productionValue
        referenceValue
        |@ sprintf "is not the same as reference implementation, ref: %A, prod: %A"
            referenceValue productionValue

let specs (calculator: SlopeAndAspectCalculator) x = 
    (``slope is value from 0 to 90 degrees and aspect from 0 to 360 degrees or None if some heights are missing`` x calculator)
    .&. (``is 90-degrees symmetric`` x calculator)
    .&. (``calculates the same value as the reference implementation`` x calculator)

[<Fact>]
let ``The reference slope implementation adheres to all the properties``() =
    let genHeight = floatInRange -100 500
    let genHeightMaybe = genHeight |> optionOfWithFrequency 1
    let genHeightsWindow = Gen.arrayOfLength 4 genHeightMaybe

    genHeightsWindow |> Arb.fromGen 
    |> Prop.forAll 
    <| specs referenceSlopeAndAspectCalculator
    |> Check.QuickThrowOnFailure
    //|> replayPropertyCheck (1444256201,296656141)

[<Fact>]
let ``Production slope and aspect implementation adheres to all the properties``() =
    let genHeight = floatInRange -100 500
    let genHeightMaybe = genHeight |> optionOfWithFrequency 1
    let genHeightsWindow = Gen.arrayOfLength 4 genHeightMaybe

    genHeightsWindow |> Arb.fromGen 
    |> Prop.forAll 
    <| specs calculateSlopeAndAspect
    |> Check.QuickThrowOnFailure
