/// <summary>
/// Contains common geometry types and functions.
/// </summary>
module Demeton.Geometry.Common

open System

/// <summary>
/// Represents a point in 2D space.
/// </summary>
type Point = float * float

/// <summary>
/// Represents a bounding box in 2D space.
/// </summary>
type Bounds = { MinX: float; MinY: float; MaxX: float; MaxY: float } with
    /// <summary>
    /// Represents an empty bounding box.
    /// </summary>
    static member Empty = { 
        MinX = Double.MaxValue
        MinY = Double.MaxValue
        MaxX = Double.MinValue
        MaxY = Double.MinValue 
        }

/// <summary>
/// Represents a point defined by a longitude and latitude.
/// </summary>
type LonLat = Point

/// <summary>
/// Represents a bounding box defined by a minimum and maximum 
/// longitude and latitude.
/// </summary>
type LonLatBounds = { 
    MinLon: float
    MinLat: float
    MaxLon: float
    MaxLat: float
    }

/// <summary>
/// Splits a flat list of x and y coordinates into two separate lists, one for
/// x coordinates and the other for y coordinates.
/// </summary>
let splitCoords (coords: 'T list)
    = List.foldBack (fun x (l, r) -> x::r, l) coords ([], [])

/// <summary>
/// Creates a list of <see cref="Point" /> tuples from a flat list of 
/// coordinates.
/// </summary>
let floatsListToPoints (floatsList: float list): Point list =
    let (xList, yList) = floatsList |> splitCoords
    List.zip xList yList

/// <summary>
/// Determines whether a list of points are all inside the specified box.
/// </summary>
let areAllPointsInsideBox minX minY maxX maxY (points: Point list) =
    points 
    |> List.exists (fun (x, y) -> 
        x < minX || x >= maxX || y < minY || y >= maxY)
    |> not

/// <summary>
/// Normalizes the value of angle between 0 and <see cref="normalizer" />.
/// </summary>
let normalizeAngle (angle: float) (normalizer: float) =
    let angleRemainder = angle % normalizer
    match angleRemainder with
    | x when x < 0. -> angleRemainder + normalizer
    | _ -> angleRemainder

/// <summary>
/// Calculates the absolute difference between two angles using the specified 
/// normalizer (typically, a value of 360 or 2*Pi).
/// </summary>
let differenceBetweenAngles 
    (angle1: float) (angle2: float) (normalizer: float) =
    let diff = normalizeAngle angle1 normalizer 
                - normalizeAngle angle2 normalizer
    let diffAbs = abs diff

    if diffAbs > normalizer / 2. then normalizer - diffAbs
    else diffAbs

// todo doc
let inline degToRad deg = deg * Math.PI / 180.
let inline radToDeg rad = rad * 180. / Math.PI

