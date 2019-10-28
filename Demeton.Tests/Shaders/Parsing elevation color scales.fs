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
    | NoNoneColor of string
    | UnsortedScale of string
    | InvalidScale of string


let ``elevation color scale parsing properties`` scaleCase =
    match scaleCase with
    | ValidScale (scaleString, expectedColorScale) ->
        let scaleParseResult = ElevationColoring.tryParseScale scaleString
        scaleParseResult = Result.Ok expectedColorScale
        |> Prop.classify true "valid scale"
        |> Prop.label "parsed scale is as expected"
    | NoMarks scaleString ->
        let scaleParseResult = ElevationColoring.tryParseScale scaleString
        scaleParseResult = Result.Error "invalid color scale"
        |> Prop.classify true "scale without any marks"
        |> Prop.label "scale must have at least one mark"        
    | UnsortedScale scaleString ->
        let scaleParseResult = ElevationColoring.tryParseScale scaleString
        scaleParseResult = Result.Error "color scale marks are not sorted"
        |> Prop.classify true "unsorted scale"
        |> Prop.label "scale marks must be sorted"
    | NoNoneColor scaleString ->
        let scaleParseResult = ElevationColoring.tryParseScale scaleString
        scaleParseResult = Result.Error "invalid color scale"
        |> Prop.classify true "color scale without none color"
        |> Prop.label "scale must adhere to the format"
    | InvalidScale scaleString ->
        let scaleParseResult = ElevationColoring.tryParseScale scaleString
        scaleParseResult = Result.Error "invalid color scale"
        |> Prop.classify true "invalid color scale"
        |> Prop.label "scale must adhere to the format"
        
let removeNoneFromString (scaleString: string) =
    let i = scaleString.IndexOf "none="
    match i with
    | 0 -> ""
    | _ -> scaleString.Substring(0, i - 1)

let indexesOfChar chr (value: string): int [] =
    value |> Seq.toArray
    |> Array.indexed
    |> Array.filter (fun (index, strChr) -> strChr = chr)
    |> Array.map (fun (index, _) -> index)

let insertIntoString (textToInsert: string) index (text: string) =
    text.Substring(0, index) + textToInsert + text.Substring(index)

let insertRandomWhitespace (scaleString: string) =
    let genSelectedChar = Gen.elements (scaleString |> indexesOfChar ';')
    genSelectedChar |> Gen.map (fun index -> 
        scaleString |> insertIntoString "  " index)

[<Fact>]
let ``Testing properties of elevation color scale parsing``() =
    let genColor = Gen.frequency [ 
        (1, ColorGen.colorWith1Alpha); (8, ColorGen.color)]

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
                Marks = scale.Marks |> ElevationColoring.sortScaleMarks })

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
                let sortedMarks = 
                    scale.Marks |> ElevationColoring.sortScaleMarks
                let marksAreSorted = scale.Marks = sortedMarks
                match marksAreSorted with
                | true -> ValidScale (scaleString, scale)
                | false -> UnsortedScale scaleString 
            )

    let genNoNoneColor =
        genScaleAndString
        |> Gen.map (fun (scaleString, _) -> 
            let scaleStringWithoutNone = removeNoneFromString scaleString
            NoNoneColor scaleStringWithoutNone)

    let genCase = Gen.frequency [
        (6, genInvalidScale); 
        (6, genCaseFromScale) 
        (1, genNoNoneColor) 
        ]

    genCase |> Arb.fromGen
    |> Prop.forAll <| ``elevation color scale parsing properties``
    |> Check.QuickThrowOnFailure
    //|> replayPropertyCheck (2117183665,296662558)
