/// Contains functions that work on line segments.
module Demeton.Geometry.LineSegment

open System

/// Active pattern for distinguishing between various line segment cases.
let (|Horizontal|Vertical|SinglePoint|Other|) = function
    | ((x1, y1), (x2, y2)) when x1 = x2 && y1 = y2 -> SinglePoint
    | ((x1, _), (x2, _)) when x1 = x2 -> Horizontal
    | ((_, y1), (_, y2)) when y1 = y2 -> Vertical
    | _ -> Other

/// Returns a point in the middle of a line segment.
let midpoint ((x1, y1), (x2, y2)) = ((x1 + x2) / 2., (y1 + y2) / 2.)

/// Returns the length of the line segment.
let length ((x1, y1), (x2, y2)) =
    let dx = x2 - x1
    let dy = y2 - y1
    Math.Sqrt (dx * dx + dy * dy)

/// Extends the line segment by moving the second endpoint by the specified
/// factor in the same direction.
let extend (factor: float) ((x1, y1), (x2, y2)) =
    match (x1, y1), (x2, y2) with
    | Horizontal ->
        let x2' = (x2 - x1) * factor + x1
        (x1, y1), (x2', y1)
    | Vertical ->
        let y2' = (y2 - y1) * factor + y1
        (x1, y1), (x2, y2')
    | SinglePoint -> (x1, y1), (x1, y1)
    | Other ->
        let ratio = (x2 - x1) / (y2 - y1)
        let y2' = (y2 - y1) * factor + y1
        let x2' = (y2' - y1) * ratio + x1
        (x1, y1), (x2', y2')

