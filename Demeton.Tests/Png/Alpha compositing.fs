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


let ``Compositing properties``(source, dest) =
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
        | _ -> 
            let composedA = Rgba8Bit.a composed
            composedA >= sourceA 
            |> Prop.label "composed alpha >= source alpha"
            |@ sprintf "%d <= %d <= %d" sourceA composedA destA

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
