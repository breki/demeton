[<RequireQualifiedAccess>]
module Demeton.Geometry.Bounds

open Demeton.Geometry.Common

let multiply factor bounds =
    { 
        MinX = bounds.MinX * factor
        MinY = bounds.MinY * factor
        MaxX = bounds.MaxX * factor
        MaxY = bounds.MaxY * factor
    }

let isInsideInclusive x y bounds =
    x >= bounds.MinX && x <= bounds.MaxX && y >= bounds.MinY && y <= bounds.MaxY

let extendWith x y bounds =
    if isInsideInclusive x y bounds then bounds
    else { 
            MinX = min bounds.MinX x
            MinY = min bounds.MinY y
            MaxX = max bounds.MaxX x
            MaxY = max bounds.MaxY y
        }

let mbrOf points =
    points 
    |> Seq.fold (fun mbr (x, y) -> mbr |> extendWith x y) Bounds.Empty
