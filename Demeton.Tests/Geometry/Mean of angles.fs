module Tests.Hillshading.``Mean of angles``

open Demeton.Geometry.Common

open System
open Xunit
open FsCheck
open TestHelp
open PropertiesHelp

[<Literal>]
let Tolerance = 1.E-10

let addAngles angle1 angle2 =
    normalizeAngle (angle1 + angle2) (Math.PI * 2.)

let ``averaging single value returns the same value`` (angles: float[]) =
    angles.Length = 1
    ==> lazy
        let meanAngle = angles |> meanOfAngles Tolerance
        let normalizedMean = normalizeAngle meanAngle (Math.PI * 2.)
        let normalizedSingle = normalizeAngle angles.[0] (Math.PI * 2.)

        (normalizedMean =~= normalizedSingle)
        |> Prop.label "averaging single value"

let ``adding before averaging or after is same`` (angles, angleToAdd) =
    let additionBefore =
        angles
        |> Array.map (fun x -> addAngles x angleToAdd)
        |> meanOfAngles Tolerance

    let additionAfter = addAngles (meanOfAngles Tolerance angles) angleToAdd

    match (Double.IsNaN additionBefore, Double.IsNaN additionAfter) with
    | true, true -> true |@ sprintf ""
    | true, false ->
        false
        |@ sprintf
            "additionBefore %f <> additionAfter %f"
            additionBefore
            additionAfter
    | false, true ->
        false
        |@ sprintf
            "additionBefore %f <> additionAfter %f"
            additionBefore
            additionAfter
    | _ ->
        let additionBeforeNormalized =
            normalizeAngle additionBefore (Math.PI * 2.)

        let additionAfterNormalized =
            normalizeAngle additionAfter (Math.PI * 2.)

        additionBeforeNormalized =~= additionAfterNormalized
        |@ sprintf
            "additionBefore %f <> additionAfter %f"
            additionBeforeNormalized
            additionAfterNormalized

let ``mean of opposing angles is NaN`` angles =
    let oppositeAngles = angles |> Array.map (fun angle -> angle + Math.PI)
    let combinedAngles = Array.append angles oppositeAngles

    Double.IsNaN(meanOfAngles Tolerance combinedAngles)
    |> Prop.label "mean of opposing angles is NaN"

[<Fact>]
let ``Mean of angles properties`` () =
    let genAngle = floatInRange 0 360 |> Gen.map degToRad

    let genSingleAngle = Gen.arrayOfLength 1 genAngle

    genSingleAngle |> Arb.fromGen |> Prop.forAll
    <| ``averaging single value returns the same value``
    |> Check.QuickThrowOnFailure

    let genAngles = Gen.arrayOf genAngle

    genAngles |> Arb.fromGen |> Prop.forAll
    <| ``mean of opposing angles is NaN``
    |> Check.QuickThrowOnFailure

    Gen.map2 (fun x y -> (x, y)) genAngles genAngle
    |> Arb.fromGen
    |> Prop.forAll
    <| ``adding before averaging or after is same``
    |> Check.QuickThrowOnFailure
