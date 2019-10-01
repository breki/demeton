module Demeton.Geometry

type Point = float * float
type LonLat = Point

type Bounds = { 
    MinLon: float
    MinLat: float
    MaxLon: float
    MaxLat: float
    }

let splitCoords (coords: float list)
    = List.foldBack (fun x (l, r) -> x::r, l) coords ([], [])

let floatsListToPoints (floatsList: float list): Point list =
    let (xList, yList) = floatsList |> splitCoords
    List.zip xList yList

let validatePointsInsideBox minX minY maxX maxY (points: Point list) =
    invalidOp "todo"