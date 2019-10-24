// https://en.wikipedia.org/wiki/Blend_modes
// https://en.wikipedia.org/wiki/Alpha_compositing
// https://www.w3.org/TR/compositing/
// http://ssp.impulsetrain.com/porterduff.html
// https://keithp.com/~keithp/porterduff/p253-porter.pdf
// https://de.wikipedia.org/wiki/Porter-Duff_Composition

module Tests.Png.``Alpha compositing``

open Png

open Xunit
open FsCheck

//type ColorPremultiplied = {
//    A: float
//    RGB: (float * float * float)
//}

//let multiply value (color: ColorPremultiplied)
//    =

////let inline alpha color = float (Rgba8Bit.a color) / 255.
//let inline toPremultiplied color = 
//    let alpha = float (Rgba8Bit.a color) / 255.

//    (
//        float (Rgba8Bit.r color) / 255.,
//        float (Rgba8Bit.g color) / 255.,
//        float (Rgba8Bit.b color) / 255.,
//    )

//let inline premultiply

//let alphaCompositing 
//    (source: Rgba8Bit.RgbaColor) 
//    (destination: Rgba8Bit.RgbaColor): Rgba8Bit.RgbaColor =
    
//    let sourceAlpha = alpha source
//    let destAlpha = alpha destination

//    let outAlpha = sourceAlpha + destAlpha * (1 - sourceAlpha)

//    (Rgba8Bit.r source) * sourceAlpha

//    //let r = 
//    //    (float (Rgba8Bit.r source)) * aSource
//    //    + 
    
let over source dest = 
    match (Rgba8Bit.a source, Rgba8Bit.a dest) with
    | (0uy, _) -> dest
    | (_, 0uy) -> source
    | _ -> 0u

let ``Compositing properties``(source, dest) =
    let composed = over source dest

    let sourceA = Rgba8Bit.a source
    let destA = Rgba8Bit.a dest

    let props = 
        match (sourceA, destA) with
        | (0uy, _) -> 
            composed = dest |> Prop.label "source alpha = 0 -> composed = dest"
        | (_, 0uy) -> 
            composed = source |> Prop.label "dest alpha = 0 -> composed = source"
        | _ -> true |> Prop.label "not covered yet"

    props 
    |> Prop.classify (sourceA = 0uy) "sourceA = 0"
    |> Prop.classify (destA = 0uy) "destA = 0"

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
