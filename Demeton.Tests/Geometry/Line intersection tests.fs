module Tests.Geometry.``Line intersection tests``

open Demeton.Geometry.LineSegment

open Xunit
open FsCheck
open PropertiesHelp

type LineIntersectionPropertyTest
    (output: Xunit.Abstractions.ITestOutputHelper) =

    let tolerance = 0.0001
            
    let areLineSegmentsSame line1 line2 = line1 = line2
    let areLineSegmentsOpposite (p1, p2) (p3, p4) = p1 = p4 && p2 = p3
            
    let isCollinear p1 p2 p3 = left tolerance (area2 p1 p2 p3) = Collinear
            
    let oneOfVerticesIsCollinear (p1, p2) (p3, p4) =
        let collinear1 = p3 |> isCollinear p1 p2
        let collinear2 = p4 |> isCollinear p1 p2
        xor collinear1 collinear2 

    let sharingOneVertex line1 line2 =
        let ((p1, p2), (p3, p4)) = (line1, line2)
        ((p1 = p3 || p1 = p4 || p2 = p3 || p2 = p4)
         && not (areLineSegmentsOpposite line1 line2)
         && not (areLineSegmentsSame line1 line2))
                
    let oneSegmentVertexIsOnOtherSegment line1 line2 =
        (not (sharingOneVertex line1 line2))
        && (
            ((line2 |> oneOfVerticesIsCollinear line1)
             || (line1 |> oneOfVerticesIsCollinear line2))
        )
        
    let liesOnSegment p1 p2 p3 =
        between (left tolerance (area2 p1 p2 p3)) p1 p2 p3
        
    let isCollinearOverlappingWith (p1, p2) (p3, p4) =
        isCollinear p1 p2 p3 && isCollinear p1 p2 p4
        && ((liesOnSegment p1 p2 p3) || (liesOnSegment p1 p2 p4))
            
    let liesOnTheSameSideOf (p1, p2) (p3, p4) =
        match (left tolerance (area2 p1 p2 p3)),
            (left tolerance (area2 p1 p2 p4)) with
        | (Left, Left) -> true
        | (Right, Right) -> true
        | _ -> false
            
    let ``line intersection detection properties`` (line1, line2) =
        
        let intersectionResult = doLineSegmentsIntersect tolerance line1 line2
        let ((p1, p2), (p3, p4)) = (line1, line2)
        
        let ``returned result matches the situation`` =
            match intersectionResult with
            | Same ->
                (areLineSegmentsSame line1 line2)
                |> Prop.classify true "Same"
                |> Prop.label "two line segments are the same"
            | Opposite ->
                (areLineSegmentsOpposite line1 line2)
                |> Prop.classify true "Opposite"
                |> Prop.label "two line segments are opposites of each other"
            | SharingOneVertex ->
                sharingOneVertex line1 line2
                |> Prop.classify true "SharingOneVertex"
                |> Prop.label "two line segments share one of the vertices"
            | OneVertexLiesOnOtherSegment ->
                oneSegmentVertexIsOnOtherSegment line1 line2
                |> Prop.classify true "OneVertexLiesOnOtherSegment"
                |> Prop.label "one (and only one) line segment's vertex lines on other line segment"
            | CollinearOverlapping ->
                ((line1 |> isCollinearOverlappingWith line2)
                 || (line2 |> isCollinearOverlappingWith line1))
                |> Prop.classify true "CollinearOverlapping"
                |> Prop.label "two line segments are collinear and overlapping"
            | NotIntersect ->
                ((line1 |> liesOnTheSameSideOf line2)
                 || (line2 |> liesOnTheSameSideOf line1))
                |> Prop.classify true "NotIntersect"
                |> Prop.label "two line segments do not intersect"
            | Intersect ->
                let intersectionPoint =
                    findLineSegmentsIntersection
                        intersectionResult line1 line2
                
                match intersectionPoint with
                | None ->
                    false
                    |> Prop.label "intersecting segments, but there is no intersection point"
                | Some point ->
                    ((between (left tolerance (area2 p1 p2 point)) p1 p2 point)
                        && between (left tolerance (area2 p3 p4 point)) p3 p4 point)
                    |> Prop.label "intersection point does not lie on line segments"

        let ``situation matches the returned result`` =
            match line1, line2 with
            | _ when areLineSegmentsSame line1 line2 ->
                intersectionResult = Same
                |> Prop.label
                       "two line segments are same, but the intersection function did not detect that"
            | _ when areLineSegmentsOpposite line1 line2 ->
                intersectionResult = Opposite
                |> Prop.label
                       "two line segments are opposite, but the intersection function did not detect that"
            | _ when sharingOneVertex line1 line2 ->
                intersectionResult = SharingOneVertex
                |> Prop.label
                       "two line segments share one of the vertices, but the intersection function did not detect that"
            | _ when oneSegmentVertexIsOnOtherSegment line1 line2 ->
                intersectionResult = OneVertexLiesOnOtherSegment
                |> Prop.label
                       "one (and only one) line segment's vertex lines on other line segment, but the intersection function did not detect that"
            | _ when ((line1 |> isCollinearOverlappingWith line2)
                    || (line2 |> isCollinearOverlappingWith line1)) ->
                intersectionResult = CollinearOverlapping
                |> Prop.label
                       "two line segments are collinear and overlapping, but the intersection function did not detect that"
            | _ when ((line1 |> liesOnTheSameSideOf line2)
                    || (line2 |> liesOnTheSameSideOf line1)) ->
                intersectionResult = NotIntersect
                |> Prop.label
                       "two line segments do not intersect, but the intersection function did not detect that"                
            | _ -> true |> Prop.classify true "not covered yet"
        
        (``returned result matches the situation``
        .&. ``situation matches the returned result``)
        |@ sprintf "intersection function returned %A" intersectionResult

//type LineIntersectionTestCase =
//    | CollinearNonOverlapping 
//    | CollinearHorizontal
//    | CollinearVertical
//    | Parallel
//    | ParallelHorizontal of (LineSegment * LineSegment)
//    | ParallelVertical of (LineSegment * LineSegment)
//    | RandomIntersecting
//    | RandomNonIntersecting
// Random
     
    [<Fact>]
    member this.``Test line intersection detection properties``() =
        let genCoord = floatInRange 0 100
        let genPoint = Gen.zip genCoord genCoord
        let genLine = Gen.zip genPoint genPoint
        let genLinePair = Gen.zip genLine genLine
        
        let genSameCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                (((x1, y1), (x2, y2)), ((x1, y1), (x2, y2))) ) 
        
        let genOppositeCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                (((x1, y1), (x2, y2)), ((x2, y2), (x1, y1))) ) 
        
        let genSharingVertexCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), (_, (x4, y4))) ->
                (((x1, y1), (x2, y2)), ((x2, y2), (x4, y4))) ) 
        
        let genOneVertexCollinearCase =
            genLinePair
            |> Gen.map (fun (line1, (_, p4)) ->
                let pointOnSegment1 = line1 |> extend 0.75 |> snd
                (line1, (pointOnSegment1, p4)) )
        
        let genCollinearOverlappingCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                let line1 = ((x1, y1), (x2, y2))
                
                let p3 = midpoint line1
                let line2 = extend 3. (p3, (x2, y2))
                
                (line1, line2) ) 
        
        let genParallelHorizontalCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, _)), ((x3, y3), (x4, _))) ->
                (((x1, y1), (x2, y1)), ((x3, y3), (x4, y3))) ) 
        
        let genParallelVerticalCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (_, y2)), ((x3, y3), (_, y4))) ->
                (((x1, y1), (x1, y2)), ((x3, y3), (x3, y4))) ) 
        
        let genRandomCase = genLinePair
        
        let gen = Gen.frequency [
            (1, genSameCase) 
            (1, genOppositeCase) 
            (2, genOneVertexCollinearCase) 
            (2, genCollinearOverlappingCase) 
            (2, genSharingVertexCase) 
            (2, genParallelHorizontalCase) 
            (2, genParallelVerticalCase)
            (3, genRandomCase)
        ]
        
        ``line intersection detection properties``
        |> checkPropertyWithTestSize gen output 1000 1000 
//        |> replayPropertyCheck gen output (565776821,296679728)