module Demeton.Geometry.Common

open System

type Point = float * float

type Bounds = { MinX: float; MinY: float; MaxX: float; MaxY: float } with
    static member Empty = { 
        MinX = Double.MaxValue
        MinY = Double.MaxValue
        MaxX = Double.MinValue
        MaxY = Double.MinValue 
        }

type LonLat = Point

type LonLatBounds = { 
    MinLon: float
    MinLat: float
    MaxLon: float
    MaxLat: float
    }

let splitCoords (coords: 'T list)
    = List.foldBack (fun x (l, r) -> x::r, l) coords ([], [])

let floatsListToPoints (floatsList: float list): Point list =
    let (xList, yList) = floatsList |> splitCoords
    List.zip xList yList

let areAllPointsInsideBox minX minY maxX maxY (points: Point list) =
    points 
    |> List.exists (fun (x, y) -> 
        x < minX || x >= maxX || y < minY || y >= maxY)
    |> not

