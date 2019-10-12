module Tests.Shaders.Hillshading.``Igor's shading method``

open Demeton.Geometry.Common
open Demeton.Shaders.Hillshading

open System

open Xunit
open FsCheck
open PropertiesHelp

let ``Uses transparency for totally flat area`` (parameters, slope, aspect) =
    let color = igorHillshade parameters 0. slope aspect
       
    let flatArea = Double.IsNaN(aspect)

    match flatArea with
    | false -> 
        true 
        |> Prop.classify true "Non-flat area"
        |> Prop.label "Area is not flat"
    | true -> 
        color.A = 0uy 
            |> Prop.classify true "Flat area"
            |> Prop.label "Uses transparency for totally flat area"

[<Fact>]
let ``Igor's shading properties``() =
    let genCircleAngle = floatInRange 0 360 |> Gen.map degToRad

    let genSunAzimuth = genCircleAngle
    let genShadingIntensity = floatInRange 1 10
    let genColorComponent = Arb.from<byte> |> Arb.toGen
    let genColor = genColorComponent |> Gen.arrayOfLength 3
    let genParameters = 
        Gen.zip3 genSunAzimuth genShadingIntensity genColor
        |> Gen.map (fun (az, shint, col) -> 
            { SunAzimuth = az; ShadingIntensity = shint; 
                ShadingColorR = col.[0]; 
                ShadingColorG = col.[1]; 
                ShadingColorB = col.[2] })

    let genAspect = 
        genCircleAngle
        |> optionOfWithFrequency 5
        |> Gen.map (
            fun angleMaybe -> 
                match angleMaybe with
                | None -> Double.NaN
                | Some angle -> angle)

    let genSlope = floatInRange 0 90 |> Gen.map degToRad

    let genAllParameters = Gen.zip3 genParameters genSlope genAspect
    genAllParameters |> Arb.fromGen
    |> Prop.forAll <| ``Uses transparency for totally flat area``
    |> Check.QuickThrowOnFailure
