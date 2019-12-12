module Tests.Geometry.``Polygon self intersection tests``

open Demeton.Geometry.Common
open Demeton.Geometry.LineSegmentsIntersection

open Xunit
open FsCheck
open PropertiesHelp

type Polygon = {
    Points: Point list
}

type PolygonSelfIntersectionResult =
    | Intersecting
    | NonIntersecting
    | InvalidPolygon

let isPolygonSelfIntersecting tolerance polygon =
    match polygon.Points with
    | points when points.Length < 3 -> InvalidPolygon
    | points -> 
        let indexedEdges =
            [ points |> List.head ]
            |> List.append points 
            |> List.pairwise
            |> List.mapi (fun i line -> (i, line))

        let edgesCount = indexedEdges.Length
        
        let areEdgesNeighbors edge1Id edge2Id =
            match abs (edge1Id - edge2Id) with
            | 1 -> true
            | diff when diff = edgesCount - 1 -> true
            | diff when diff > edgesCount - 1 ->
                invalidOp "bug: this should never happen"
            | _ -> false
        
        let edgesIntersect
            ((edge1Id, edge1): (int * LineSegment))
            ((edge2Id, edge2): (int * LineSegment)) =
            match edge1Id, edge2Id with
            | _ when edge1Id = edge2Id -> false
            | _ when areEdgesNeighbors edge1Id edge2Id -> false
            | _ ->
                match doLineSegmentsIntersect tolerance edge1 edge2 with
                | LineSegmentsIntersectionDetectionResult.IntersectProperly ->
                    true
                | LineSegmentsIntersectionDetectionResult.NotIntersect ->
                    false
                | result ->
                    invalidOp (sprintf "todo: intersect result: %A" result)
        
        let isSelfIntersecting =
            indexedEdges
            |> List.exists (fun e1 ->
                indexedEdges
                |> List.exists (fun e2 -> edgesIntersect e1 e2))
        if isSelfIntersecting then Intersecting
        else NonIntersecting

let ``polygon properties`` (points: Point[]) =
    let tolerance = 0.00001
    
    let polygon = { Points = points |> Array.toList }
    let result = isPolygonSelfIntersecting tolerance polygon

    let ``polygon with less than 3 points is invalid`` =
        if points.Length < 3 then
            result = InvalidPolygon
            |> Prop.classify true "invalid polygon"
            |> Prop.label "the function did not detect an invalid polygon"
        else
            true |> Prop.classify true "valid polygon"

    ``polygon with less than 3 points is invalid``
    |> Prop.classify (points.Length = 3) "triangle"
    |> Prop.classify (result = Intersecting) "self-intersecting"
    |> Prop.classify (result = NonIntersecting) "non-self-intersecting"

type SelfIntersectingPolygonTests (output: Xunit.Abstractions.ITestOutputHelper) =
    [<Fact>]
    member  this.``Icebreaker``() =
        let genCoord = floatInRange -100 100
        let genPoint = Gen.zip genCoord genCoord

        let genPoints = Gen.arrayOf genPoint
    
        ``polygon properties``    
        |> PropertiesHelp.checkPropertyWithTestSize genPoints output 1000 100  
