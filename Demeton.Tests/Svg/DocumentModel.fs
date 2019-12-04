module rec Svg.DocumentModel

open Svg.Paths

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

type Path = {
    Id: ElementName
    Style: Style option
    Classes: Classes option
    PathData: PathData
}

type GroupChildElement =
    | Path of Path
    | Group of Group

type Group = {
    Id: ElementName
    Style: Style option
    Classes: Classes option
    Children: GroupChildElement list
}

type Document = {
    Style: Style option
    Classes: Classes option
    Width: SvgLength
    Height: SvgLength
    ViewBox: ViewBox
    Children: GroupChildElement list
}
