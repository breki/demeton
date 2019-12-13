module Demeton.Geometry.PolygonSelfIntersection

open Demeton.Geometry.Common
open Demeton.Geometry.LineSegmentsIntersection

type PolygonEdge = (int * LineSegment)

let edgesCount polygon = polygon.Vertices.Length

let indexedEdges polygon: PolygonEdge list =
    [ polygon.Vertices |> List.head ]
    |> List.append polygon.Vertices 
    |> List.pairwise
    |> List.filter (fun edge -> edge |> LineSegment.length > 0.)
    |> List.mapi (fun i line -> (i, line))

let areEdgesNeighbors polygon edge1Id edge2Id =
    let edgesCount = edgesCount polygon
    
    match abs (edge1Id - edge2Id) with
    | 1 -> true
    | diff when diff = edgesCount - 1 -> true
    | diff when diff > edgesCount - 1 ->
        invalidOp "bug: this should never happen"
    | _ -> false

type EventY = float

type EdgeEnters = { Y: EventY; Edge: PolygonEdge }
type EdgeExits = { Y: EventY; Edge: PolygonEdge }

type EdgeEvent = EdgeEnters of EdgeEnters | EdgeExits of EdgeExits

type PolygonSelfIntersectionResult =
    | Intersecting
    | NonIntersecting
    | InvalidPolygon

/// A function that determines whether the two polygon edges (represented by
/// their line segments) intersect or not.
type EdgesIntersectFunc = LineSegment -> LineSegment -> bool

/// The default implementation of EdgesIntersectFunc. 
let edgesIntersectDefaultFunc tolerance: EdgesIntersectFunc =
    fun edge1Segment edge2Segment ->
    match doLineSegmentsIntersect tolerance edge1Segment edge2Segment with
    | LineSegmentsIntersectionDetectionResult.IntersectProperly ->
        true
    | LineSegmentsIntersectionDetectionResult.NotIntersect -> false
    | LineSegmentsIntersectionDetectionResult.OneEndpointLiesOnOtherSegment ->
        false
    | LineSegmentsIntersectionDetectionResult.SharingOneEndpoint ->
        false
    | result -> invalidOp (sprintf "todo: handle case %A" result) 

let isPolygonSelfIntersecting edgesIntersectFunc polygon =
    let enterEvent edge: EdgeEvent =
        let (_, ((_, y1), (_, y2))) = edge
        EdgeEnters { Y = min y1 y2; Edge = edge }
    let exitEvent edge: EdgeEvent =
        let (_, ((_, y1), (_, y2))) = edge
        EdgeExits { Y = max y1 y2; Edge = edge }
    
    let compareEvents (event1: EdgeEvent) (event2: EdgeEvent) =
        match event1, event2 with
        | EdgeEnters a, EdgeEnters b -> a.Y.CompareTo b.Y
        | EdgeExits a, EdgeExits b -> a.Y.CompareTo b.Y
        | EdgeEnters a, EdgeExits b ->
            let c = a.Y.CompareTo b.Y
            if c <> 0 then c
            else -1
        | EdgeExits a, EdgeEnters b ->
            let c = a.Y.CompareTo b.Y
            if c <> 0 then c
            else 1
    
    let removeEdgeFromList ((edgeIdToRemove, _): PolygonEdge) activeEdges =
        activeEdges
        |> List.tryFindIndex(fun (edgeId, _) -> edgeId = edgeIdToRemove)
        |> function
        | Some index ->
            let (before, after) = 
                activeEdges |> List.splitAt index
            after |> List.tail |> List.append before 
        | None -> invalidOp "bug: the edge was expected in the list"
//        |> Seq.filter (fun (edgeId, _) -> edgeId = edgeIdToRemove)

    let doesContainEdgeThatIntersectsWith
        (edgeId, edgeSegment) edges =
        edges
        |> Seq.exists (fun (otherEdgeId, otherEdgeSegment) ->
            if areEdgesNeighbors polygon edgeId otherEdgeId then false
            else edgesIntersectFunc edgeSegment otherEdgeSegment)
    
    let processEvent (activeEdges, foundIntersection) event =
        match foundIntersection with
        | true -> ([], true)
        | false ->
            match event with
            | EdgeEnters event ->
                 if activeEdges |> doesContainEdgeThatIntersectsWith event.Edge
                    then ([], true)
                 else
                    (event.Edge :: activeEdges, false)
            | EdgeExits event ->
                (activeEdges |> removeEdgeFromList event.Edge, false)
    
    match polygon.Vertices with
    | vertices when vertices.Length < 3 -> InvalidPolygon
    | _ ->
        let edges = polygon |> indexedEdges
        let enterEvents = edges |> Seq.map enterEvent
        let exitEvents = edges |> Seq.map exitEvent
        
        let allEvents =
            Seq.append enterEvents exitEvents
            |> Seq.sortWith compareEvents
        
        let (_, isSelfIntersecting) =
            allEvents |> Seq.fold processEvent ([], false)
        
        if isSelfIntersecting then Intersecting
        else NonIntersecting