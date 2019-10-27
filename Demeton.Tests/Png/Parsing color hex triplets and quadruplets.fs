module Tests.Png.``Parsing color hex triplets and quadruplets``

open Png

open FParsec
open FParsec.CharParsers

open Xunit
open FsCheck

let runParser(str: string) (parsingFunc: Parser<'u, unit>) =
    use charStream = new CharStream<unit>(str, 0, str.Length)

    let reply = parsingFunc charStream
    match reply.Status with
    | Ok -> Result.Ok reply.Result
    | _ -> Result.Error()


let parseColorHexValue hexValue =
    invalidOp "todo"
    //let hexParser = hex

    //let hexColor = 
    //    pstring "#" 
    //    >>. manyMinMaxSatisfy 6 8 isHex 
    //    |>> many |>> hex
        

    //let result = runParser hexValue hexColor
    //match result 

    //let parser = pstring "#"

    //CharParsers.

    //let parseHash = FParsec.CharParsers.pchar '#'
    //let parseHexByte = 
    //    FParsec.Primitives.pipe2 
    //        FParsec.CharParsers.hex FParsec.CharParsers.hex
    //let parseHexBytes = 
    //    FParsec.Primitives.pipe4 
    //        parseHexByte parseHexByte parseHexByte parseHexByte 

let ``Color hex properties`` color =
    let hexValue = Rgba8Bit.toHex color

    let parsedColor = parseColorHexValue hexValue

    let fullOpacity = (Rgba8Bit.a color) = 0xffuy

    let propInverse = 
        parsedColor = color
        |> Prop.label "parsing hex value does returns the same color"

    let propLength =
        match fullOpacity with
        | true -> 
            hexValue.Length = 7
            |> Prop.label "full opacity hex value has 1+6 characters"
        | false ->
            hexValue.Length = 9
            |> Prop.label "full opacity hex value has 1+8 characters"
        
    (propInverse .&. propLength)
    |> Prop.classify fullOpacity "A = FF"

[<Fact(Skip="todo")>]
let ``Testing hex color properties``() =
    let genByte = Arb.generate<byte>

    let genRandomColor = Arb.generate<Rgba8Bit.RgbaColor>
    let genColorWith1Alpha = 
        genByte |> Gen.arrayOfLength 3
        |> Gen.map (fun components -> 
            Rgba8Bit.rgbaColor components.[0] components.[1] components.[2] 255uy)

    let genColor = Gen.frequency [ 
        (1, genColorWith1Alpha); (8, genRandomColor)]

    genColor |> Arb.fromGen
    |> Prop.forAll <| ``Color hex properties``
    |> Check.QuickThrowOnFailure