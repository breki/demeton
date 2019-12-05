module rec Svg.DocumentModel

type Unit =
    | Mm

type SvgLength = {
    Number: float
    Unit: Unit option
}

type ViewBox = {
    MinX: float
    MinY: float
    Width: float
    Height: float
}

type ClassName = string
type Classes = ClassName list

type Style = {
    Style: string
}

type ElementName = ElementName of string

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

type GroupChildElement =
    | Group of Group
    | Path of Path

type Group = {
    Id: ElementName
    Style: Style option
    Classes: Classes option
    Children: GroupChildElement list
}

type Path = {
    Id: ElementName
    Style: Style option
    Classes: Classes option
    PathData: PathData
}

type Document = {
    Style: Style option
    Classes: Classes option
    Width: SvgLength
    Height: SvgLength
    ViewBox: ViewBox
    Children: GroupChildElement list
}
