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
    | NoMarks of string
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

let sortScaleMarks marks =
    marks |> Array.sortBy (fun (elevation, _) -> elevation)

let tryParseScale value: Result<ElevationColoring.ColorScale, string> =
    match value with
    | null -> Result.Error "invalid color scale"
    | _ -> 
        let result = run parseScale value

        match result with
        | Success(scale, _, _) -> 
            let sortedMarks = scale.Marks |> sortScaleMarks
            let marksAreSorted = scale.Marks = sortedMarks
            match marksAreSorted with
            | true -> Result.Ok scale
            | false -> Result.Error "color scale marks are not sorted"

        | _ -> Result.Error "invalid color scale"

let ``elevation color scale parsing properties`` scaleCase =
    match scaleCase with
    | ValidScale (validScaleString, expectedColorScale) ->
        let scaleParseResult = tryParseScale validScaleString
        scaleParseResult = Result.Ok expectedColorScale
        |> Prop.classify true "valid scale"
        |> Prop.label "parsed scale is as expected"
    | NoMarks scaleString ->
        let scaleParseResult = tryParseScale scaleString
        scaleParseResult = Result.Error "invalid color scale"
        |> Prop.classify true "scale without any marks"
        |> Prop.label "scale must have at least one mark"        
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
        |> Gen.map (fun rgb -> 
            Rgba8Bit.rgbaColor rgb.[0] rgb.[1] rgb.[2] 255uy)

    let genColor = Gen.frequency [ 
        (1, genColorWith1Alpha); (8, genRandomColor)]

    let genInvalidScale = 
        Arb.generate<string> |> Gen.map (fun x -> InvalidScale x)

    let genElevation = 
        Gen.choose (-1000, 5000) |> Gen.map (fun x -> DemHeight (int16 x))
    let genMark = 
        Gen.zip genElevation genColor 
        |> Gen.map (fun (elevation, color) -> 
            ElevationColoring.ColorScaleMark (elevation, color))
    let genMarks = Gen.arrayOf genMark

    let buildScale marks noneColor: ElevationColoring.ColorScale =
        { Marks = marks; NoneColor = noneColor; }

    let genScale: Gen<ElevationColoring.ColorScale> =
        Gen.zip genMarks genColor
        |> Gen.map (fun (marks, noneColor) -> buildScale marks noneColor)

    let genSortedScale =
        genScale
        |> Gen.map (fun scale -> 
            { scale with 
                Marks = scale.Marks 
                |> Array.sortBy (fun (elevation, _) -> elevation) })

    let genSortedAndUnsortedScale =
        Gen.frequency [ (5, genSortedScale); (1, genScale) ]

    let genScaleAndString: Gen<string * ElevationColoring.ColorScale> =
        genSortedAndUnsortedScale
        |> Gen.map (fun scale -> 
            (ElevationColoring.colorScaleToString scale, scale))

    let genCaseFromScale = 
        genScaleAndString |> Gen.map (fun (scaleString, scale) -> 
            match scale.Marks with
            | [||] -> NoMarks scaleString
            | _ -> 
                let sortedMarks = scale.Marks |> sortScaleMarks
                let marksAreSorted = scale.Marks = sortedMarks
                match marksAreSorted with
                | true -> ValidScale (scaleString, scale)
                | false -> UnsortedScale scaleString 
            )

    let genCase = Gen.frequency [(2, genInvalidScale); (2, genCaseFromScale) ]

    genCase |> Arb.fromGen
    |> Prop.forAll <| ``elevation color scale parsing properties``
    |> Check.QuickThrowOnFailure
    //|> replayPropertyCheck (2117183665,296662558)
