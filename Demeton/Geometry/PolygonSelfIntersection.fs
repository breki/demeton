/// Contains code for determining whether a polygon is self-intersecting.
///
/// The isPolygonSelfIntersecting function uses a modification of the
/// Shamos-Hoey Algorithm, which itself is a simplification of
/// Bentley–Ottmann algorithm
/// (https://en.wikipedia.org/wiki/Bentley%E2%80%93Ottmann_algorithm),
/// basically running a horizontal sweep line over the polygon which
/// reduces the needed number of intersection checks between edges.
///
/// Note that currently the algorithm uses a list instead of a binary search
/// tree to keep the edges in the status, so the efficiency is still O(n^2).
///
/// Some relevant links:
/// - https://stackoverflow.com/questions/4876065/check-if-polygon-is-self-intersecting
/// - https://www.webcitation.org/6ahkPQIsN
/// - http://geomalgorithms.com/a09-_intersect-3.html
/// - https://web.archive.org/web/20141211224415/http://www.lems.brown.edu/~wq/projects/cs252.html
module Demeton.Geometry.PolygonSelfIntersection

open Demeton.Geometry.Common
open Demeton.Geometry.Polygon
open Demeton.Geometry.LineSegmentsIntersection

/// The y-coordinate of the plane sweep event.
type private EventY = float

/// The plane sweep event which occurs when the sweep line reaches the top
/// of an edge. The edge then enters the plane sweep status.
type private EdgeEnters = { Y: EventY; Edge: PolygonEdge }
/// The plane sweep event which occurs when the sweep line reaches the bottom
/// of an edge. The edge then exits the plane sweep status.
type private EdgeExits = { Y: EventY; Edge: PolygonEdge }

/// The plane sweep event.
type private EdgeEvent =
    /// The plane sweep event which occurs when the sweep line reaches the top
    /// of an edge. The edge then enters the plane sweep status.
    | EdgeEnters of EdgeEnters
    /// The plane sweep event which occurs when the sweep line reaches the
    /// bottom of an edge. The edge then exits the plane sweep status.
    | EdgeExits of EdgeExits

/// The result of the polygon self-intersection detection algorithm.
type PolygonSelfIntersectionResult =
    /// The polygon is self-intersecting.
    | Intersecting
    /// The polygon is not self-intersecting.
    | NonIntersecting
    /// The polygon is invalid - it has less than 3 vertices.
    | InvalidPolygon

/// A function that determines whether the two polygon edges (represented by
/// their line segments) intersect or not.
type EdgesIntersectFunc = LineSegment -> LineSegment -> bool

/// The default implementation of EdgesIntersectFunc which treats all
/// intersection results except IntersectProperly as non-intersecting. This may
/// need to be changed in the future, once this code starts being used in
/// other algorithms.
let edgesIntersectDefaultFunc tolerance : EdgesIntersectFunc =
    fun edge1Segment edge2Segment ->
        match doLineSegmentsIntersect tolerance edge1Segment edge2Segment with
        | LineSegmentsIntersectionDetectionResult.IntersectProperly -> true
        | LineSegmentsIntersectionDetectionResult.NotIntersect -> false
        | LineSegmentsIntersectionDetectionResult.OneEndpointLiesOnOtherSegment ->
            false
        | LineSegmentsIntersectionDetectionResult.SharingOneEndpoint -> false
        | result -> invalidOp (sprintf "todo: handle case %A" result)

/// Determines whether the given polygon is self-intersecting.
let isPolygonSelfIntersecting edgesIntersectFunc polygon =
    let enterEvent edge : EdgeEvent =
        let _, ((_, y1), (_, y2)) = edge
        EdgeEnters { Y = min y1 y2; Edge = edge }

    let exitEvent edge : EdgeEvent =
        let _, ((_, y1), (_, y2)) = edge
        EdgeExits { Y = max y1 y2; Edge = edge }

    /// Comparison function for events used by the plane sweep algorithm
    /// to process the events in the right order.
    let compareEvents (event1: EdgeEvent) (event2: EdgeEvent) =
        match event1, event2 with
        | EdgeEnters a, EdgeEnters b -> a.Y.CompareTo b.Y
        | EdgeExits a, EdgeExits b -> a.Y.CompareTo b.Y
        | EdgeEnters a, EdgeExits b ->
            let c = a.Y.CompareTo b.Y
            if c <> 0 then c else -1
        | EdgeExits a, EdgeEnters b ->
            let c = a.Y.CompareTo b.Y
            if c <> 0 then c else 1

    /// Removes the specified edge from the edges status list.
    let removeEdgeFromList ((edgeIdToRemove, _): PolygonEdge) activeEdges =
        activeEdges
        |> List.tryFindIndex (fun (edgeId, _) -> edgeId = edgeIdToRemove)
        |> function
            | Some index -> activeEdges |> DataStructures.ListEx.removeAt index
            | None -> invalidOp "bug: the edge was expected in the list"

    /// Determines whether the given sequence of edges contains any edge that
    /// intersects with a specified edge.
    let doesContainEdgeThatIntersectsWith (edgeId, edgeSegment) edges =
        edges
        |> Seq.exists (fun (otherEdgeId, otherEdgeSegment) ->
            if areEdgesNeighbors polygon edgeId otherEdgeId then
                false
            else
                edgesIntersectFunc edgeSegment otherEdgeSegment)

    /// Processes the next plane sweep event, based on the current state.
    /// The current state consists of a list of "active" edges (the ones in the
    /// plane sweep status) and "foundIntersection" flag that serves as way
    /// to exit the plane sweep algorithm early (once we have found an
    /// intersection, we do not need to examine the remaining events).
    let processEvent (activeEdges, foundIntersection) event =
        match foundIntersection with
        | true -> ([], true)
        | false ->
            match event with
            | EdgeEnters event ->
                if
                    activeEdges |> doesContainEdgeThatIntersectsWith event.Edge
                then
                    ([], true)
                else
                    (event.Edge :: activeEdges, false)
            | EdgeExits event ->
                (activeEdges |> removeEdgeFromList event.Edge, false)

    match polygon.Vertices with
    | vertices when vertices.Length < 3 -> InvalidPolygon
    | _ ->
        let edges = polygon |> polygonEdges

        // creates enter and exit events...
        let enterEvents = edges |> Seq.map enterEvent
        let exitEvents = edges |> Seq.map exitEvent

        /// ...merges them together and sorts them
        let allEvents =
            Seq.append enterEvents exitEvents |> Seq.sortWith compareEvents

        // runs the main plane sweep algorithm
        let _, isSelfIntersecting =
            allEvents |> Seq.fold processEvent ([], false)

        if isSelfIntersecting then Intersecting else NonIntersecting
