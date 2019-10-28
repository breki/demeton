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

let ``Color premultiplication properties`` color =
    let alpha = AlphaCompositing.byteToRatio (Rgba8Bit.a color)

    let premultiplied = AlphaCompositing.toPremultiplied color
    let fromPremultiplied = 
        AlphaCompositing.toRgbaColor premultiplied alpha
    match alpha with
    | 0. -> true |> Prop.classify true "A=0"
    | _ -> 
        (color = fromPremultiplied)
        |> Prop.classify true "A>0"
        |> Prop.label "Color premultiplication is invertible when A>0"
        |@ sprintf "%s <> %s" 
            (Rgba8Bit.toHex fromPremultiplied) (Rgba8Bit.toHex color) 


[<Fact>]
let ``Color premultiplication``() =
    let genColor = Gen.frequency [ 
        (1, ColorGen.colorWith0Alpha); 
        (1, ColorGen.colorWith1Alpha); 
        (8, ColorGen.color)]

    genColor
    |> Arb.fromGen
    |> Prop.forAll <| ``Color premultiplication properties``
    |> Check.QuickThrowOnFailure

let ``Alpha compositing over operation properties``(source, dest) =
    let composed = AlphaCompositing.pixelOver source dest

    let sourceA = Rgba8Bit.a source
    let destA = Rgba8Bit.a dest

    let props = 
        match (sourceA, destA) with
        | (0uy, _) -> 
            composed = dest 
            |> Prop.label "source alpha = 0 -> composed = dest"
        | (255uy, _) -> 
            composed = source 
            |> Prop.label "source alpha = 1 -> composed = source"
        | (_, 0uy) -> 
            composed = source 
            |> Prop.label "dest alpha = 0 -> composed = source"
        | (_, 255uy) -> 
            Rgba8Bit.a composed = 255uy 
            |> Prop.label "dest alpha = 1 -> composed alpha = 1"
        | _ -> true |> Prop.classify true "mixed colors"

    let composedA = Rgba8Bit.a composed

    let props2 = 
        props .&. (
            composedA >= sourceA 
            |> Prop.label "composed alpha >= source alpha"
            |@ sprintf "%d <= %d <= %d" sourceA composedA destA)

    props2 
    |> Prop.classify (sourceA = 0uy) "sourceA = 0"
    |> Prop.classify (sourceA = 255uy) "sourceA = 1"
    |> Prop.classify (destA = 0uy) "destA = 0"
    |> Prop.classify (destA = 255uy) "destA = 1"

[<Fact>]
let ``Alpha compositing over operation``() =
    let genColor = Gen.frequency [ 
        (1, ColorGen.colorWith0Alpha); 
        (1, ColorGen.colorWith1Alpha); 
        (8, ColorGen.color)]

    Gen.zip genColor genColor
    |> Arb.fromGen
    |> Prop.forAll <| ``Alpha compositing over operation properties``
    |> Check.QuickThrowOnFailure
