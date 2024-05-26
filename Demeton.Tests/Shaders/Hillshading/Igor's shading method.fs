module Tests.Shaders.Hillshading.``Igor's shading method``

open Demeton.Geometry.Common
open Demeton.Shaders
open Png

open System

open Xunit
open FsCheck
open PropertiesHelp

let private darkness ofColor = Rgba8Bit.a ofColor

let private byteAbsDiff (value1: byte) (value2: byte) : byte =
    match value1 <= value2 with
    | true -> value2 - value1
    | false -> value1 - value2

let ``Igor shading properties``
    (
        parameters: IgorHillshader.ShaderParameters,
        aspect1,
        aspect2
    ) =
    let sunAzimuth = parameters.SunAzimuth
    //    let sunAltitude = degToRad 45.
    let slope45 = degToRad 45.

    let colorOfFlatFace = IgorHillshader.shadePixel parameters 0. 0. aspect1

    let prop1 =
        (Rgba8Bit.a colorOfFlatFace = 0uy)
        |> Prop.label "flat face has minimum darkness"

    let aspect1Darkness =
        IgorHillshader.shadePixel parameters 0. slope45 aspect1 |> darkness

    let aspect2Darkness =
        IgorHillshader.shadePixel parameters 0. slope45 aspect2 |> darkness

    let aspect1SunAzimuthDiff =
        differenceBetweenAngles aspect1 sunAzimuth (Math.PI * 2.)

    let aspect2SunAzimuthDiff =
        differenceBetweenAngles aspect2 sunAzimuth (Math.PI * 2.)

    let prop4 =
        if aspect1SunAzimuthDiff <= aspect2SunAzimuthDiff then
            aspect1Darkness <= aspect2Darkness
        else
            aspect1Darkness >= aspect2Darkness
        |> Prop.label "aspect closer to sun azimuth should not be darker"
        |@ ($"Sun azimuth: %g{radToDeg sunAzimuth}, "
            + $"aspect1 = %g{radToDeg aspect1} "
            + $"(diff %g{radToDeg aspect1SunAzimuthDiff}), "
            + $"aspect2 = %g{radToDeg aspect2} "
            + $"(diff %g{radToDeg aspect2SunAzimuthDiff}), "
            + $"darkness1 = %d{aspect1Darkness}, "
            + $"darkness2 = %d{aspect2Darkness}")

    let aspectOneSide = sunAzimuth - aspect1SunAzimuthDiff
    let aspectOtherSide = sunAzimuth + aspect1SunAzimuthDiff

    let darknessOneSide =
        IgorHillshader.shadePixel parameters 0. slope45 aspectOneSide
        |> darkness

    let darknessOtherSide =
        IgorHillshader.shadePixel parameters 0. slope45 aspectOtherSide
        |> darkness

    let prop5 =
        (byteAbsDiff darknessOneSide darknessOtherSide <= 1uy)
        |> Prop.label
            "faces facing the sun at the same angle have the same darkness"
        |@ $"%d{darknessOneSide} <> %d{darknessOtherSide}"

    prop1 .&. prop4 .&. prop5

[<Fact>]
let ``Igor shading properties test`` () =
    let generateParameters
        (
            az,
            col: Rgba8Bit.RgbaColor
        ) : IgorHillshader.ShaderParameters =
        { SunAzimuth = az
          SunAltitude = IgorHillshader.DefaultSunAltitude
          ShadingColor = col
          Intensity = 1.
          HeightsArrayIndex = 0 }

    let genCircleAngle = floatInRange 0 360 |> Gen.map degToRad

    let genSunAzimuth = genCircleAngle

    let genParameters =
        Gen.zip genSunAzimuth ColorGen.color |> Gen.map generateParameters

    //    let genAspect =
    //        genCircleAngle
    //        |> optionOfWithFrequency 5
    //        |> Gen.map (fun angleMaybe ->
    //            match angleMaybe with
    //            | None -> Double.NaN
    //            | Some angle -> angle)

    let genSlope = floatInRange 0 90 |> Gen.map degToRad

    let genAllParameters = Gen.zip3 genParameters genSlope genSlope

    genAllParameters |> Arb.fromGen |> Prop.forAll
    <| ``Igor shading properties``
    |> Check.QuickThrowOnFailure
