module Tests.Geometry.``Polygon self intersection tests``

open Demeton.Geometry.Common
open Demeton.Geometry.Polygon
open Demeton.Geometry.PolygonSelfIntersection

open Xunit
open FsCheck
open PropertiesHelp

/// A brute-force (O(n^2)) implementation of polygon self-intersection
/// detection, used as a test oracle for the more efficient implementation.
let isPolygonSelfIntersectingBrute edgesIntersectFunc polygon =
    match polygon.Vertices with
    | vertices when vertices.Length < 3 -> InvalidPolygon
    | _ -> 
        let edges = polygon |> polygonEdges

        let edgesIntersect
            ((edge1Id, edge1): (int * LineSegment))
            ((edge2Id, edge2): (int * LineSegment)) =
            match edge1Id, edge2Id with
            | _ when edge1Id = edge2Id -> false
            | _ when areEdgesNeighbors polygon edge1Id edge2Id -> false
            | _ -> edgesIntersectFunc edge1 edge2
        
        let isSelfIntersecting =
            edges
            |> Seq.exists (fun e1 ->
                edges |> Seq.exists (edgesIntersect e1))
        if isSelfIntersecting then Intersecting
        else NonIntersecting

let ``polygon properties`` (vertices: Point[]) =
    let tolerance = 0.00001
    let verticesCount = vertices.Length
        
    let polygon = { Vertices = vertices |> Array.toList }
    let resultBrute =
        isPolygonSelfIntersectingBrute
            (edgesIntersectDefaultFunc tolerance) polygon
    let result =
        isPolygonSelfIntersecting
            (edgesIntersectDefaultFunc tolerance) polygon

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
    let genLikelyIntersectingPolygon genCoord = 
        let genPoint = Gen.zip genCoord genCoord
        Gen.arrayOf genPoint
    
    /// Generates vertices for a non-intersecting polygon (in most cases)
    /// by generating a set of points on a circle (with each point having
    /// a random angle and a random radius). The points are then sorted by
    /// angle and converted to cartesian coordinates.
    /// Source for the idea:
    /// https://stackoverflow.com/questions/8997099/algorithm-to-generate-random-2d-polygon
    let genLikelyNonIntersectingPolygon genCoord =
        let genAngle = floatInRange 0 360 |> Gen.map degToRad
        let genRadius = genCoord
        let genPointOnCircle = Gen.zip genAngle genRadius
        Gen.arrayOf genPointOnCircle
        |> Gen.map (fun unorderedPoints ->
            unorderedPoints
            |> Array.sortBy (fun (angle, _) -> angle)
            |> Array.map (fun (angle, radius) ->
                let x = radius * cos angle
                let y = radius * sin angle
                (x, y) ) )

    let genAxisAlignedRectangle genCoord =
        let genX = genCoord
        let genY = genCoord
        let genWidth = genCoord
        let genHeight = genCoord
        Gen.zip3 (Gen.zip genX genY) genWidth genHeight
        |> Gen.map (fun ((x, y), w, h) ->
            [| (x, y); (x + w, y); (x + w, y + h); (x, y + h) |])

    /// Generates a shape that has an intersection with one edge being
    /// horizontal.
    let genHorizontalIntersection genCoord =
        let genX = genCoord
        let genY = genCoord
        let genDX = genCoord
        let genDY = genCoord
        Gen.zip3 (Gen.zip genX genY) genDX genDY
        |> Gen.map (fun ((x, y), dx, dy) ->
            [| (x, y); (x, dy * 2.)
               (x + dx * 2., y + dy); (x - dx * 2., y + dy) |])
    
    member this.runTests genCoord =
        let gen =
            Gen.frequency[
                (10, genLikelyIntersectingPolygon genCoord)
                (10, genLikelyNonIntersectingPolygon genCoord)
                (1, genAxisAlignedRectangle genCoord)
                (1, genHorizontalIntersection genCoord)
            ]
        
        ``polygon properties``    
        |> PropertiesHelp.checkPropertyWithTestSize gen output 1000 100
//        |> PropertiesHelp.replayPropertyCheck gen output (675502539,296692176)
        
    [<Fact>]
    member  this.``Test polygon self-intersecting properties (small floats)``() =
        this.runTests (floatInRange 0 100)
        
    [<Fact>]
    member  this.``Test polygon self-intersecting properties (dense ints)``() =
        this.runTests (Gen.choose (0, 100) |> (Gen.map float))
        
    [<Fact>]
    member  this.``Test polygon self-intersecting properties (big floats)``() =
        this.runTests (floatInRange 1000000000 1000000000)
