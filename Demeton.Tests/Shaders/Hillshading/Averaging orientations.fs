module Tests.Hillshading.``Averaging orientations``

open Demeton.Geometry.Common

open System
open Xunit
open FsCheck
open TestHelp
open PropertiesHelp

let addAngle orientation angle =
    normalizeAngle (orientation + angle) (Math.PI * 2.)

let orientationsAverage orientations =
    match orientations with
    | [||] -> Double.NaN
    | _ -> 
        let (totalX, totalY) =
            orientations
            |> Array.fold (
                fun (vx, vy) orient -> 
                    let orientX = Math.Cos (orient)
                    let orientY = Math.Sin (orient)
                    (vx + orientX, vy + orientY))
                (0., 0.)

        let meanOrientation = Math.Atan2(totalY, totalX)
        meanOrientation

let ``averaging single value returns the same value`` (orientations: float[]) =
    orientations.Length = 1 ==> lazy
        let meanOrientation = orientations |> orientationsAverage
        let normalizedMean = normalizeAngle meanOrientation (Math.PI * 2.)
        let normalizedSingle = normalizeAngle orientations.[0] (Math.PI * 2.)
        
        (normalizedMean =~= normalizedSingle) 
        |> Prop.label "averaging single value"

let ``adding before averaging or after is same`` (orientations, angle) =
    let additionBefore = 
        orientations 
        |> Array.map (fun x -> addAngle x angle)
        |> orientationsAverage
    let additionAfter =
        addAngle (orientationsAverage orientations) angle

    match (Double.IsNaN additionBefore, Double.IsNaN additionAfter) with
    | (true, true) -> true |@ sprintf ""
    | (true, false) -> 
        false
        |@ sprintf "additionBefore %f <> additionAfter %f" 
            additionBefore additionAfter
    | (false, true) -> 
        false
            |@ sprintf "additionBefore %f <> additionAfter %f" 
                additionBefore additionAfter
    | _ -> 
            let additionBeforeNormalized = 
                normalizeAngle additionBefore (Math.PI * 2.)
            let additionAfterNormalized = 
                normalizeAngle additionAfter (Math.PI * 2.)
            additionBeforeNormalized =~= additionAfterNormalized
                |@ sprintf "additionBefore %f <> additionAfter %f" 
                additionBeforeNormalized additionAfterNormalized

[<Fact>]
let ``Orientation averaging properties``() =
    let genOrientation = floatInRange 0 360 |> Gen.map degToRad

    let genSingleOrientation = Gen.arrayOfLength 1 genOrientation

    genSingleOrientation |> Arb.fromGen
    |> Prop.forAll <| ``averaging single value returns the same value``
    |> Check.QuickThrowOnFailure

    let genOrientations = Gen.arrayOf genOrientation

    Gen.map2 (fun x y -> (x, y)) genOrientations genOrientation
    |> Arb.fromGen
    |> Prop.forAll <| ``adding before averaging or after is same``
    |> Check.QuickThrowOnFailure
