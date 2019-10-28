module Tests.Png.``Parsing color hex triplets and quadruplets``

open Png

open Xunit
open FsCheck

let ``Color hex properties`` color =
    let hexValue = Rgba8Bit.toHex color

    let parsedColorMaybe = Rgba8Bit.tryParseColorHexValue hexValue

    let fullOpacity = (Rgba8Bit.a color) = 0xffuy

    let propInverse = 
        parsedColorMaybe = Result.Ok color
        |> Prop.label "parsing hex value returns the same color"

    let propLength =
        match fullOpacity with
        | true -> 
            hexValue.Length = 7
            |> Prop.label "full opacity hex value has 1+6 characters"
        | false ->
            hexValue.Length = 9
            |> Prop.label "full opacity hex value has 1+8 characters"
        
    let propLowercase =
        let parsedColorMaybe = 
            Rgba8Bit.tryParseColorHexValue (hexValue.ToLower())
        parsedColorMaybe = Result.Ok color
        |> Prop.label "supports lowercase hex values"
        
    let propUppercase =
        let parsedColorMaybe = 
            Rgba8Bit.tryParseColorHexValue (hexValue.ToUpper())
        parsedColorMaybe = Result.Ok color
        |> Prop.label "supports uppercase hex values"

    (propInverse .&. propLength .&. propLowercase .&. propUppercase)
    |> Prop.classify fullOpacity "A = 0xFF"

[<Fact>]
let ``Testing hex color properties``() =
    let genColor = Gen.frequency [ 
        (1, ColorGen.colorWith1Alpha); (8, ColorGen.color)]

    genColor |> Arb.fromGen
    |> Prop.forAll <| ``Color hex properties``
    |> Check.QuickThrowOnFailure