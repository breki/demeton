module Tests.Geometry.``Polygon self intersection tests``

open Demeton.Geometry.Common
open Demeton.Geometry.LineSegmentsIntersection
open Demeton.Geometry.PolygonSelfIntersection

open Xunit
open FsCheck
open PropertiesHelp

/// A brute-force (O(n^2)) implementation of polygon self-intersection
/// detection, used as a test oracle for the more efficient implementation.
let isPolygonSelfIntersectingBrute tolerance polygon =
    match polygon.Vertices with
    | vertices when vertices.Length < 3 -> InvalidPolygon
    | _ -> 
        let indexedEdges = polygon |> indexedEdges

        let edgesIntersect
            ((edge1Id, edge1): (int * LineSegment))
            ((edge2Id, edge2): (int * LineSegment)) =
            match edge1Id, edge2Id with
            | _ when edge1Id = edge2Id -> false
            | _ when areEdgesNeighbors polygon edge1Id edge2Id -> false
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
                indexedEdges |> List.exists (edgesIntersect e1))
        if isSelfIntersecting then Intersecting
        else NonIntersecting

let ``polygon properties`` (vertices: Point[]) =
    let tolerance = 0.00001
    let verticesCount = vertices.Length
        
    let polygon = { Vertices = vertices |> Array.toList }
    let resultBrute = isPolygonSelfIntersectingBrute tolerance polygon
    let result = isPolygonSelfIntersecting tolerance polygon

    let ``polygon with less than 3 vertices is invalid`` =
        if verticesCount < 3 then
            resultBrute = InvalidPolygon
            |> Prop.classify true "invalid polygon"
            |> Prop.label "the function did not detect an invalid polygon"
        else 
            (resultBrute <> InvalidPolygon)
            |> Prop.label
                   "the function reported an invalid polygon even though it has 3 vertices or more"

    let ``polygon self-intersection detection is correct`` =
        result = resultBrute
        |> Prop.label "the production function result does not match the brute function"

    ``polygon with less than 3 vertices is invalid``
    .&. ``polygon self-intersection detection is correct``
    |> Prop.classify (verticesCount = 3) "triangle"
    |> Prop.classify (verticesCount > 3 && verticesCount < 20)
           "polygon with < 20 vertices"
    |> Prop.classify (verticesCount >= 20) "polygon with >= 20 vertices"
    |> Prop.classify (resultBrute = Intersecting) "self-intersecting"
    |> Prop.classify (resultBrute = NonIntersecting) "non-self-intersecting"

type SelfIntersectingPolygonTests (output: Xunit.Abstractions.ITestOutputHelper) =
    let genLikelyIntersectingPolygon() = 
        let genCoord = floatInRange -100 100
        let genPoint = Gen.zip genCoord genCoord
        Gen.arrayOf genPoint
    
    /// Generates vertices for a non-intersecting polygon (in most cases)
    /// by generating a set of points on a circle (with each point having
    /// a random angle and a random radius). The points are then sorted by
    /// angle and converted to cartesian coordinates.
    /// Source for the idea:
    /// https://stackoverflow.com/questions/8997099/algorithm-to-generate-random-2d-polygon
    let genLikelyNonIntersectingPolygon() =
        let genAngle = floatInRange 0 360 |> Gen.map degToRad
        let genRadius = floatInRange 0 100
        let genPointOnCircle = Gen.zip genAngle genRadius
        Gen.arrayOf genPointOnCircle
        |> Gen.map (fun unorderedPoints ->
            unorderedPoints
            |> Array.sortBy (fun (angle, _) -> angle)
            |> Array.map (fun (angle, radius) ->
                let x = radius * cos angle
                let y = radius * sin angle
                (x, y) ) )
    
    [<Fact>]
    member  this.``Test polygon self-intersecting properties``() =
        let gen =
            Gen.frequency[
                (1, genLikelyIntersectingPolygon())
                (1, genLikelyNonIntersectingPolygon())
            ]
        
        ``polygon properties``    
        |> PropertiesHelp.checkPropertyWithTestSize gen output 1000 100  
