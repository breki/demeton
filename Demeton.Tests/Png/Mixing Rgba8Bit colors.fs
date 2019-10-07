module Tests.Png.``Mixing Rgba8Bit colors``

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit
open PropertiesHelp

open Png

let colorA = Rgba8Bit.rgbaColor 10uy 20uy 30uy 40uy
let colorB = Rgba8Bit.rgbaColor 110uy 120uy 130uy 140uy

[<Fact>]
let ``If mixing ratio is 0 return the first color exactly``() =
    test <@ Rgba8Bit.mixColors colorA colorB 0. = colorA @>

[<Fact>]
let ``If mixing ratio is 1 return the second color exactly``() =
    test <@ Rgba8Bit.mixColors colorA colorB 1. = colorB @>

[<Fact>]
let ``If mixing ratio is between 0 and 1 return the mixed color``() =
    test <@ Rgba8Bit.mixColors colorA colorB 0.5 = 
        Rgba8Bit.rgbaColor 60uy 70uy 80uy 90uy @>

[<Property>]
let ``Color distance properties``() =
    let genColor1 = Arb.generate<Rgba8Bit.RgbaColor>
    let genColor2 = Arb.generate<Rgba8Bit.RgbaColor>

    let colorDistanceIsCommutative(color1, color2) =
        Rgba8Bit.colorDistance color1 color2 =
            Rgba8Bit.colorDistance color2 color1

    let properties (color1, color2)
        = colorDistanceIsCommutative(color1, color2)

    Gen.map2 (fun x y -> x, y) genColor1 genColor2
    |> Arb.fromGen
    |> Prop.forAll <| properties


let isBetweenTwoOriginalColors (color1, color2, mixRatio) =
    let mixedColor = Rgba8Bit.mixColors color1 color2 mixRatio

    valueIsBetweenInclusive 
        "red component" 
        (Rgba8Bit.r color1) (Rgba8Bit.r color2) (Rgba8Bit.r mixedColor)
    .&. valueIsBetweenInclusive 
        "green component" 
        (Rgba8Bit.g color1) (Rgba8Bit.g color2) (Rgba8Bit.g mixedColor)
    .&. valueIsBetweenInclusive 
        "blue component" 
        (Rgba8Bit.b color1) (Rgba8Bit.b color2) (Rgba8Bit.b mixedColor)
    .&. valueIsBetweenInclusive 
        "alpha component" 
        (Rgba8Bit.a color1) (Rgba8Bit.a color2) (Rgba8Bit.a mixedColor)

let swappingColorsProducesTheSameResult (color1, color2, mixRatio) =
    let x = Rgba8Bit.mixColors color1 color2 mixRatio
    let y = Rgba8Bit.mixColors color2 color1 (1. - mixRatio)
    x .=. y

let distancesBetweenMixedColorAreProportionalToTheMixRatio
    (color1, color2, mixRatio) =
    let mixedColor = Rgba8Bit.mixColors color1 color2 mixRatio
    let distanceTo1 = Rgba8Bit.colorDistance color1 mixedColor
    let distanceTo2 = Rgba8Bit.colorDistance color2 mixedColor
    let totalDistance = distanceTo1 + distanceTo2

    printfn 
        "color1: %s, color2: %s, mixed: %s, d1=%f, d2=%f, dt=%f"
        (Rgba8Bit.toHex color1)
        (Rgba8Bit.toHex color2)
        (Rgba8Bit.toHex mixedColor)
        distanceTo1
        distanceTo2
        totalDistance

    totalDistance >= 100. ==> lazy
        let actualMixRatio = distanceTo1 / totalDistance
        let propertyHolds = abs (mixRatio - actualMixRatio) < 0.05 

        if not propertyHolds then System.Diagnostics.Debugger.Break()

        propertyHolds |@ sprintf 
            "wrong mix ratio: expected %f, got %f" mixRatio actualMixRatio

[<Property(Verbose=false)>]
let ``Color mixing properties``() =
    let genColor1 = Arb.generate<Rgba8Bit.RgbaColor>
    let genColor2 = Arb.generate<Rgba8Bit.RgbaColor>
    let genMixRatio = floatFrom0To1Inclusive 10000

    let properties (color1, color2, mixRatio)
        = isBetweenTwoOriginalColors (color1, color2, mixRatio)
            .&. swappingColorsProducesTheSameResult (color1, color2, mixRatio)
            .&. distancesBetweenMixedColorAreProportionalToTheMixRatio 
                (color1, color2, mixRatio)

    Gen.map3 (fun x y z -> x, y, z) genColor1 genColor2 genMixRatio
    |> Arb.fromGen
    |> Prop.forAll <| properties
