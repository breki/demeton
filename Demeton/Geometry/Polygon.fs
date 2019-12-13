/// Contains polygon types and functions.
module Demeton.Geometry.Polygon

open Demeton.Geometry.Common

/// A tuple representing a polygon edge, consisting of the edge ID and
/// the edge's line segment.
type PolygonEdge = (int * LineSegment)

/// Returns the total number of edges for a given polygon.
let edgesCount polygon = polygon.Vertices.Length

/// Returns edges of a given polygon.
let polygonEdges polygon: PolygonEdge seq =
    [ polygon.Vertices |> Seq.head ]
    |> Seq.append polygon.Vertices 
    |> Seq.pairwise
    |> Seq.filter (fun edge -> edge |> LineSegment.length > 0.)
    |> Seq.mapi (fun i line -> (i, line))

/// Based on two edges' IDs, determines whether they are neighbors.
let areEdgesNeighbors polygon edge1Id edge2Id =
    let edgesCount = edgesCount polygon
    
    match abs (edge1Id - edge2Id) with
    | 1 -> true
    | diff when diff = edgesCount - 1 -> true
    | diff when diff > edgesCount - 1 ->
        invalidOp "bug: this should never happen"
    | _ -> false
    