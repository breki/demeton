module Tests.Shaders.``Parsing elevation color scales``

open Demeton.Shaders
open Demeton.DemTypes

open FParsec

open Xunit
open FsCheck
open Png
open PropertiesHelp

type ScaleCase =
    | ValidScale of (string * ElevationColoring.ColorScale)
    | UnsortedScale of string
    | InvalidScale of string

type ParsedMark =
    | Mark of ElevationColoring.ColorScaleMark
    | NoneColor of Rgba8Bit.RgbaColor

let parseMark: Parser<ParsedMark, unit> =
    pipe4 pint16 (pstring "=") Rgba8Bit.hexColor (pstring ";")
        (fun elevation _ color _ -> Mark (DemHeight elevation, color))

let parseNoneColor: Parser<ParsedMark, unit> =
    pipe3 (pstring "none") (pstring "=") Rgba8Bit.hexColor
        (fun _ _ color -> NoneColor color)

let parseScale: Parser<ElevationColoring.ColorScale, unit> =
    ((many1 parseMark <?> "invalid color scale") .>>. parseNoneColor)
    |>> fun (marksUntyped, noneColorUntyped) -> 
        let marks = 
            marksUntyped 
            |> List.map (fun x ->
                match x with
                | Mark mark -> mark
                | _ -> invalidOp "bug" )
            |> List.toArray

        let noneColor = 
            match noneColorUntyped with
            | NoneColor color -> color
            | _ -> invalidOp "bug"

        let colorScale: ElevationColoring.ColorScale = 
            { Marks = marks; NoneColor = noneColor }
        colorScale

let tryParseScale value: Result<ElevationColoring.ColorScale, string> =
    match value with
    | null -> Result.Error "invalid color scale"
    | _ -> 
        let result = run parseScale value

        match result with
        | Success(result, _, _)   -> Result.Ok result
        | _ -> Result.Error "invalid color scale"

let ``elevation color scale parsing properties`` scaleCase =
    match scaleCase with
    | ValidScale (validScaleString, expectedColorScale) ->
        let scaleParseResult = tryParseScale validScaleString
        scaleParseResult = Result.Ok expectedColorScale
        |> Prop.classify true "valid scale"
        |> Prop.label "parsed scale is as expected"
    | UnsortedScale (unsortedScaleString) ->
        let scaleParseResult = tryParseScale unsortedScaleString
        scaleParseResult = Result.Error "color scale marks are not sorted"
        |> Prop.classify true "unsorted scale"
        |> Prop.label "scale marks must be sorted"
    | InvalidScale (invalidScaleString) ->
        let scaleParseResult = tryParseScale invalidScaleString

        scaleParseResult = Result.Error "invalid color scale"
        |> Prop.classify true "invalid color scale"
        |> Prop.label "scale must adhere to the format"
        
[<Fact>]
let ``Testing properties of elevation color scale parsing``() =
    let genByte = Arb.generate<byte>

    let genRandomColor = Arb.generate<Rgba8Bit.RgbaColor>
    let genColorWith1Alpha = 
        genByte |> Gen.arrayOfLength 3
        |> Gen.map (fun components -> 
            Rgba8Bit.rgbaColor components.[0] components.[1] components.[2] 255uy)

    let genColor = Gen.frequency [ 
        (1, genColorWith1Alpha); (8, genRandomColor)]

    let genInvalidScale = 
        Arb.generate<string> |> Gen.map (fun x -> InvalidScale x)

    let genElevation = Gen.choose (-1000, 5000)
    let genColorMark = Gen.zip genElevation genColor

    let scaleToString (elevation, color) noneColor =
        sprintf "%d=%s;none=%s" 
            elevation (Rgba8Bit.toHex color) (Rgba8Bit.toHex noneColor)

    let buildScale (elevation, color) noneColor: ElevationColoring.ColorScale =
        { NoneColor = noneColor; 
            Marks = [| (DemHeight (int16 elevation), color) |] }

    let genSingleMark: Gen<string * ElevationColoring.ColorScale> =
        Gen.zip genColorMark genColor 
        |> Gen.map (fun (mark, noneColor) -> 
            let scale = buildScale mark noneColor
            (ElevationColoring.colorScaleToString scale, scale))

    let genValidSingleMark = genSingleMark |> Gen.map (fun x -> ValidScale x)

    let genCase = Gen.frequency [(2, genInvalidScale); (2, genValidSingleMark) ]

    genCase |> Arb.fromGen
    |> Prop.forAll <| ``elevation color scale parsing properties``
    |> Check.QuickThrowOnFailure
    //|> replayPropertyCheck (2117183665,296662558)
