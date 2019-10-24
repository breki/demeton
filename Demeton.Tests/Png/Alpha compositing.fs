// https://en.wikipedia.org/wiki/Blend_modes
// https://en.wikipedia.org/wiki/Alpha_compositing
// https://www.w3.org/TR/compositing/
// http://ssp.impulsetrain.com/porterduff.html
// https://keithp.com/~keithp/porterduff/p253-porter.pdf
// https://de.wikipedia.org/wiki/Porter-Duff_Composition

module Tests.Png.``Alpha compositing``

open Png

open System

open Xunit
open FsCheck

let byteToRatio (byteValue: byte) = (float byteValue) / 255.
let ratioToByte (ratio: float): byte = 
    let asInt = int (Math.Round (ratio * 255.))
    byte (min (max asInt 0) 255)

type ColorRgbRatio = float * float * float

let toPremultiplied color: ColorRgbRatio =
    let a = int (Rgba8Bit.a color)
    let rPremultiplied = float (int (Rgba8Bit.r color) * a) / 65025.
    let gPremultiplied = float (int (Rgba8Bit.g color) * a) / 65025.
    let bPremultiplied = float (int (Rgba8Bit.b color) * a) / 65025.
    (rPremultiplied, gPremultiplied, bPremultiplied)

let inline addColors ((r1, g1, b1): ColorRgbRatio) ((r2, g2, b2): ColorRgbRatio)
    : ColorRgbRatio =
    ((r1 + r2), (g1 + g2), (b1 + b2))

let inline multiplyColor factor ((r, g, b): ColorRgbRatio): ColorRgbRatio =
    (r * factor, g * factor, b * factor)

let toRgbaColor ((r, g, b): ColorRgbRatio) (a: float): Rgba8Bit.RgbaColor =
    Rgba8Bit.rgbaColor
        (ratioToByte (255. * r / a))
        (ratioToByte (255. * g / a))
        (ratioToByte (255. * b / a))
        (ratioToByte a)

let over source dest = 
    let sourceAb = Rgba8Bit.a source
    let destAb = Rgba8Bit.a dest

    match (sourceAb, destAb) with
    | (0uy, _) -> dest
    | (255uy, _) -> source
    | (_, 0uy) -> source
    | (_, 255uy) -> dest
    | _ -> 
        let sourceAr = byteToRatio sourceAb
        let destAr = byteToRatio destAb

        let srcRgbR = toPremultiplied source
        let destRgbR = toPremultiplied source

        let outR = addColors srcRgbR (multiplyColor (1. - sourceAr) destRgbR)
        let outA = sourceAr + destAr * (1. - sourceAr)
        toRgbaColor outR outA

let ``Compositing properties``(source, dest) =
    let composed = over source dest

    let sourceA = Rgba8Bit.a source
    let destA = Rgba8Bit.a dest

    let props = 
        match (sourceA, destA) with
        | (0uy, _) -> 
            composed = dest |> Prop.label "source alpha = 0 -> composed = dest"
        | (255uy, _) -> 
            composed = source |> Prop.label "source alpha = 1 -> composed = source"
        | (_, 0uy) -> 
            composed = source |> Prop.label "dest alpha = 0 -> composed = source"
        | (_, 255uy) -> 
            Rgba8Bit.a composed = 255uy 
            |> Prop.label "dest alpha = 1 -> composed alpha = 1"
        | _ -> true |> Prop.label "not covered yet"

    props 
    |> Prop.classify (sourceA = 0uy) "sourceA = 0"
    |> Prop.classify (sourceA = 255uy) "sourceA = 1"
    |> Prop.classify (destA = 0uy) "destA = 0"
    |> Prop.classify (destA = 255uy) "destA = 1"

[<Fact>]
let ``Testing compositing properties``() =
    let genByte = Arb.generate<byte>

    let genRandomColor = Arb.generate<Rgba8Bit.RgbaColor>
    let genColorWith0Alpha = 
        genByte |> Gen.arrayOfLength 3
        |> Gen.map (fun components -> 
            Rgba8Bit.rgbaColor components.[0] components.[1] components.[2] 0uy)
    let genColorWith1Alpha = 
        genByte |> Gen.arrayOfLength 3
        |> Gen.map (fun components -> 
            Rgba8Bit.rgbaColor components.[0] components.[1] components.[2] 255uy)

    let genColor = Gen.frequency [ 
        (1, genColorWith0Alpha); (1, genColorWith1Alpha); (8, genRandomColor)]

    Gen.zip genColor genColor
    |> Arb.fromGen
    |> Prop.forAll <| ``Compositing properties``
    |> Check.QuickThrowOnFailure
