module Demeton.Geometry.PolygonSelfIntersection

open Demeton.Geometry.Common

type PolygonSelfIntersectionResult =
    | Intersecting
    | NonIntersecting
    | InvalidPolygon

let isPolygonSelfIntersecting tolerance polygon =
    match polygon.Vertices with
    | vertices when vertices.Length < 3 -> InvalidPolygon
    | vertices -> 
        NonIntersecting