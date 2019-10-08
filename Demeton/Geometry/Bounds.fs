/// <summary>
/// Contains functions for working with <see cref="Bounds" />.
/// </summary>
[<RequireQualifiedAccess>]
module Demeton.Geometry.Bounds

open Demeton.Geometry.Common

/// <summary>
/// Multiplies the <see cref="Bounds" /> value with the specified multiplier
/// and returns a new bounds value.
/// </summary>
let multiply factor bounds =
    { 
        MinX = bounds.MinX * factor
        MinY = bounds.MinY * factor
        MaxX = bounds.MaxX * factor
        MaxY = bounds.MaxY * factor
    }

/// <summary>
/// Determines whether a point is inside the specified bounds. Points lying
/// exactly on bounds' edges count as being inside.
/// </summary>
let isInsideInclusive x y bounds =
    x >= bounds.MinX && x <= bounds.MaxX && y >= bounds.MinY && y <= bounds.MaxY

/// <summary>
/// Returns a bounds instance that represents a minimum bounding box of the
/// specified bounds and the specified point.
/// </summary>
let extendWith x y bounds =
    if isInsideInclusive x y bounds then bounds
    else { 
            MinX = min bounds.MinX x
            MinY = min bounds.MinY y
            MaxX = max bounds.MaxX x
            MaxY = max bounds.MaxY y
        }

/// <summary>
/// Returns a minimum bounding rectangle for the specified list of points.
/// </summary>
let mbrOf points =
    points 
    |> Seq.fold (fun mbr (x, y) -> mbr |> extendWith x y) Bounds.Empty
