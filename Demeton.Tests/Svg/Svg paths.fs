module Tests.Svg.``Svg paths``

open Text
open System
open System.Text

open Xunit
open Swensen.Unquote

type Coord = float

type Angle = float

type Point = Coord * Coord

type CurveToParameters = {
    StartControlPoint: Point
    EndControlPoint: Point
    Point: Point
}

type Arc = LargeArc | SmallArc  
type Sweep = PositiveAngle | NegativeAngle

type EllipticalArc = {
    Rx : Coord
    Ry : Coord
    XAxisRotation: Angle
    Arc: Arc
    Sweep: Sweep
    Point: Point
}

type SmoothCurveToParameters = {
    EndControlPoint: Point
    Point: Point
}

type QuadraticCurveToParameters = {
    StartControlPoint: Point
    Point: Point
}

// https://www.w3.org/TR/SVG/paths.html
// https://www.w3.org/TR/SVG11/paths.html#PathData
type PathInstruction =
    | ClosePath
    | CurveToAbs of CurveToParameters list
    | CurveToRel of CurveToParameters list
    | EllipticalArcAbs of EllipticalArc list
    | EllipticalArcRel of EllipticalArc list
    | HorizLineToAbs of Coord list
    | HorizLineToRel of Coord list
    | LineToAbs of Point list
    | LineToRel of Point list
    | MoveToAbs of Point list
    | MoveToRel of Point list
    | QuadraticCurveToAbs of QuadraticCurveToParameters list
    | QuadraticCurveToRel of QuadraticCurveToParameters list
    | SmoothCurveToAbs of SmoothCurveToParameters list
    | SmoothCurveToRel of SmoothCurveToParameters list
    | SmoothQuadraticCurveToAbs of Point list
    | SmoothQuadraticCurveToRel of Point list
    | VertLineToAbs of Coord list
    | VertLineToRel of Coord list
    
type PathData = PathInstruction list

type PathDataRenderer() =
    let stringBuilder = buildString()
    
    let lastChar(): char = stringBuilder.[stringBuilder.Length - 1]
    
    member this.nextInstruction (instructionChar: char) =
        stringBuilder
        |> appendChar instructionChar
        |> ignore
        
    member this.addNumber (number: float) =
        let numberStr =
            number.ToString ("0.##", System.Globalization.CultureInfo.InvariantCulture)
            
        match lastChar, number with
        | (_, _) when Char.IsLetter (lastChar()) || number < 0. ->
            stringBuilder |> append numberStr
        | _ ->
            stringBuilder |> appendChar ' ' |> append numberStr
        |> ignore
        
    override this.ToString() = stringBuilder.ToString()

let beginNextInstruction instructionChar (pathDataRenderer: PathDataRenderer) =
    pathDataRenderer.nextInstruction instructionChar
    pathDataRenderer

let addNumber number (pathDataRenderer: PathDataRenderer) = 
    pathDataRenderer.addNumber number
    pathDataRenderer

let addPoint (x, y) (pathDataRenderer: PathDataRenderer) =
    pathDataRenderer |> addNumber x |> addNumber y

let renderCurveToParameters renderer (curveTo: CurveToParameters) =
    renderer
    |> addPoint curveTo.StartControlPoint
    |> addPoint curveTo.EndControlPoint
    |> addPoint curveTo.Point

let renderCurveTo instructionChar curves renderer =
    curves
    |> List.fold
           renderCurveToParameters
           (renderer |> beginNextInstruction instructionChar)

let renderEllipticalArc renderer (ellipticalArc: EllipticalArc) =
    renderer
    |> addNumber ellipticalArc.Rx
    |> addNumber ellipticalArc.Ry
    |> addNumber ellipticalArc.XAxisRotation
    |> addNumber (
            match ellipticalArc.Arc with
            | LargeArc -> 1.
            | SmallArc -> 0.)
    |> addNumber (
            match ellipticalArc.Sweep with
            | PositiveAngle -> 1.
            | NegativeAngle -> 0.)
    |> addPoint ellipticalArc.Point

let renderEllipticalArcs instructionChar arcs renderer =
    arcs
    |> List.fold
           renderEllipticalArc
           (renderer |> beginNextInstruction instructionChar)

let renderCoords instructionChar coords renderer =
    coords
    |> List.fold
           (fun renderer coord -> renderer |> addNumber coord)
           (renderer |> beginNextInstruction instructionChar)

let renderPathInstruction renderer instruction =
    match instruction with
    | ClosePath -> renderer |> beginNextInstruction 'Z'
    | CurveToAbs curves -> renderer |> renderCurveTo 'C' curves
    | CurveToRel curves -> renderer |> renderCurveTo 'c' curves
    | EllipticalArcAbs arcs -> renderer |> renderEllipticalArcs 'A' arcs
    | EllipticalArcRel arcs -> renderer |> renderEllipticalArcs 'a' arcs
    | HorizLineToAbs coords -> renderer |> renderCoords 'H' coords
    | HorizLineToRel coords -> renderer |> renderCoords 'h' coords
    | LineToAbs points -> renderer
    | LineToRel points -> renderer
    | MoveToAbs points -> renderer
    | MoveToRel points -> renderer
    | QuadraticCurveToAbs curves -> renderer
    | QuadraticCurveToRel curves -> renderer
    | SmoothCurveToAbs curves -> renderer
    | SmoothCurveToRel curves -> renderer
    | SmoothQuadraticCurveToAbs curves -> renderer
    | SmoothQuadraticCurveToRel curves -> renderer
    | VertLineToAbs coords -> renderer |> renderCoords 'V' coords
    | VertLineToRel coords -> renderer |> renderCoords 'v' coords

let pathDataToString pathData =
    let renderer = 
        pathData
        |> List.fold renderPathInstruction (PathDataRenderer())
    
    renderer.ToString()
    
[<Fact(Skip="todo")>]
let ``Can serialize path data to string``() =
    let curveToParameters = [
        { StartControlPoint = (10.2345,22.);
          EndControlPoint = (-33.2, 44.2); Point = (12., 44.) }
        { StartControlPoint = (12.2,22.);
          EndControlPoint = (-34.2, 44.2); Point = (15., 44.) }
    ]
    
    let ellipticalArcParameters = [
        { Rx = 10.2; Ry = 55.3; XAxisRotation = 55.4; Arc = LargeArc;
          Sweep = PositiveAngle; Point = (-22.4, 8.) }
        { Rx = 11.2; Ry = 55.3; XAxisRotation = -55.4; Arc = SmallArc;
          Sweep = NegativeAngle; Point = (22.4, 8.) }
    ]
    
    let coords = [ 10.2; 12.2; -4.3 ]
    let points = [ (1.2, 3.4); (5.6, 7.8) ]
    
    let quadCurveToParameters = [
        { StartControlPoint = (10.2,22.); Point = (12., 44.) }
        { StartControlPoint = (12.2,22.); Point = (15., 44.) }
    ]
    
    let smoothCurveToParameters = [
        { EndControlPoint = (10.2,22.); Point = (12., 44.) }
        { EndControlPoint = (12.2,22.); Point = (15., 44.) }
    ]
    
    let pathData = [
        ClosePath
        CurveToAbs curveToParameters
        CurveToRel curveToParameters
        EllipticalArcAbs ellipticalArcParameters
        EllipticalArcRel ellipticalArcParameters
        HorizLineToAbs coords
        HorizLineToRel coords
        LineToAbs points
        LineToRel points
        MoveToAbs points
        MoveToRel points
        QuadraticCurveToAbs quadCurveToParameters
        QuadraticCurveToRel quadCurveToParameters
        SmoothCurveToAbs smoothCurveToParameters
        SmoothCurveToRel smoothCurveToParameters
        SmoothQuadraticCurveToAbs points
        SmoothQuadraticCurveToRel points
        VertLineToAbs coords
        VertLineToRel coords
    ]
    
    let expectedString = 
        "Z"
        + "C10.23 22-33.2 44.2 12 44 12.2 22-34.2 44.2 15 44"
        + "c10.23 22-33.2 44.2 12 44 12.2 22-34.2 44.2 15 44"
        + "A10.2 55.3 55.4 1 1-22.4 8 11.2 55.3-55.4 0 0 22.4 8"
        + "a10.2 55.3 55.4 1 1-22.4 8 11.2 55.3-55.4 0 0 22.4 8"
        + "H10.2 12.2-4.3"
        + "h10.2 12.2-4.3"
        + "L1.2 3.4 5.6 7.8"
        + "l1.2 3.4 5.6 7.8"
        + "M1.2 3.4 5.6 7.8"
        + "Q10.2 22 12 44 12.2 22 15 44"
        + "q10.2 22 12 44 12.2 22 15 44"
        + "S10.2 22 12 44 12.2 22 15 44"
        + "s10.2 22 12 44 12.2 22 15 44"
        + "T1.2 3.4 5.6 7.8"
        + "t1.2 3.4 5.6 7.8"
        + "V1.2 3.4 5.6 7.8"
        + "v1.2 3.4 5.6 7.8"
    
    test <@ pathDataToString pathData = expectedString @>
