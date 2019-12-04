module Svg.Paths

open Text
open System

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

let renderPoints instructionChar points renderer =
    points
    |> List.fold
           (fun renderer point -> renderer |> addPoint point)
           (renderer |> beginNextInstruction instructionChar)

let renderQuadraticCurve renderer (curve: QuadraticCurveToParameters) =
    renderer
    |> addPoint curve.StartControlPoint
    |> addPoint curve.Point

let renderQuadraticCurves instructionChar curves renderer =
    curves
    |> List.fold
           renderQuadraticCurve
           (renderer |> beginNextInstruction instructionChar)

let renderSmoothCurve renderer (curve: SmoothCurveToParameters) =
    renderer
    |> addPoint curve.EndControlPoint
    |> addPoint curve.Point

let renderSmoothCurves instructionChar curves renderer =
    curves
    |> List.fold
           renderSmoothCurve
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
    | LineToAbs points -> renderer |> renderPoints 'L' points
    | LineToRel points -> renderer |> renderPoints 'l' points
    | MoveToAbs points -> renderer |> renderPoints 'M' points
    | MoveToRel points -> renderer |> renderPoints 'm' points
    | QuadraticCurveToAbs curves -> renderer |> renderQuadraticCurves 'Q' curves
    | QuadraticCurveToRel curves -> renderer |> renderQuadraticCurves 'q' curves
    | SmoothCurveToAbs curves -> renderer |> renderSmoothCurves 'S' curves
    | SmoothCurveToRel curves -> renderer |> renderSmoothCurves 's' curves
    | SmoothQuadraticCurveToAbs points -> renderer |> renderPoints 'T' points
    | SmoothQuadraticCurveToRel points -> renderer |> renderPoints 't' points
    | VertLineToAbs coords -> renderer |> renderCoords 'V' coords
    | VertLineToRel coords -> renderer |> renderCoords 'v' coords

let pathDataToString pathData =
    let renderer = 
        pathData
        |> List.fold renderPathInstruction (PathDataRenderer())
    
    renderer.ToString()
    
