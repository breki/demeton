/// <summary>
/// Contains common geometry types and functions.
/// </summary>
module Demeton.Geometry.Common

open System

/// <summary>
/// Represents a point in 2D space.
/// </summary>
type Point = float * float

/// Represents a line segment between two points.
type LineSegment = Point * Point

/// Represents a polygon.
type Polygon = { Vertices: Point list }

/// <summary>
/// Represents a bounding box in 2D space.
/// </summary>
type Bounds =
    { MinX: float
      MinY: float
      MaxX: float
      MaxY: float }

    /// <summary>
    /// Represents an empty bounding box.
    /// </summary>
    static member Empty =
        { MinX = Double.MaxValue
          MinY = Double.MaxValue
          MaxX = Double.MinValue
          MaxY = Double.MinValue }

/// <summary>
/// Represents a point defined by a longitude and latitude.
/// </summary>
type LonLat = Point

/// <summary>
/// Represents a bounding box defined by a minimum and maximum
/// longitude and latitude.
/// </summary>
type LonLatBounds =
    { MinLon: float
      MinLat: float
      MaxLon: float
      MaxLat: float }

type LongitudeDegrees = float
type LatitudeDegrees = float

let lonLatBoundsFromBounds bounds =
    { MinLon = bounds.MinX
      MinLat = bounds.MinY
      MaxLon = bounds.MaxX
      MaxLat = bounds.MaxY }

let mergeLonLatBounds
    (bounds1: LonLatBounds)
    (bounds2: LonLatBounds)
    : LonLatBounds =
    { MinLon = min bounds1.MinLon bounds2.MinLon
      MinLat = min bounds1.MinLat bounds2.MinLat
      MaxLon = max bounds1.MaxLon bounds2.MaxLon
      MaxLat = max bounds1.MaxLat bounds2.MaxLat }

/// <summary>
/// Splits a flat list of x and y coordinates into two separate lists, one for
/// x coordinates and the other for y coordinates.
/// </summary>
let splitCoords (coords: 'T list) =
    List.foldBack (fun x (l, r) -> x :: r, l) coords ([], [])

/// <summary>
/// Creates a list of <see cref="Point" /> tuples from a flat list of
/// coordinates.
/// </summary>
let floatsListToPoints (floatsList: float list) : Point list =
    let xList, yList = floatsList |> splitCoords
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
    (angle1: float)
    (angle2: float)
    (normalizer: float)
    =
    let diff =
        normalizeAngle angle1 normalizer - normalizeAngle angle2 normalizer

    let diffAbs = abs diff

    if diffAbs > normalizer / 2. then
        normalizer - diffAbs
    else
        diffAbs

/// <summary>
/// Calculates the mean of angles (in radians) using the formula from
/// <a href="https://en.wikipedia.org/wiki/Mean_of_circular_quantities#Mean_of_angles">
/// Wikipedia</a>
/// </summary>
/// <remarks>
/// The ordinary arithmetic average does not work for circular quantities like
/// angles.
/// </remarks>
/// <param name="tolerance">
/// If both the mean vector's X and Y coordinates are less than the tolerance
/// value, the function returns <see cref="Double.NaN" /> as the mean value.
/// The function also uses tolerance to round off very small angle values to
/// zero.
/// </param>
/// <returns>The mean of angles or <see cref="Double.NaN" /> if mean is
/// undefined - for example if the angles array is empty or it consists of
/// directly opposite angles.
/// </returns>
let meanOfAngles tolerance angles =
    match angles with
    | [||] -> Double.NaN
    | _ ->
        let totalX, totalY =
            angles
            |> Array.fold
                (fun (vx, vy) orient ->
                    let angleX = Math.Cos orient
                    let angleY = Math.Sin orient
                    (vx + angleX, vy + angleY))
                (0., 0.)

        if Math.Abs(totalX) < tolerance && Math.Abs(totalY) < tolerance then
            Double.NaN
        else
            let meanAngle = Math.Atan2(totalY, totalX)
            if Math.Abs(meanAngle) < tolerance then 0. else meanAngle

let inline degToRad deg = deg * Math.PI / 180.
let inline radToDeg rad = rad * 180. / Math.PI
