module Tests.Geometry.``Line intersection tests``

open Demeton.Geometry.Common
open System

open Xunit
open FsCheck
open PropertiesHelp

let xor (a: bool) (b: bool) = (not a) <> (not b)

let isZero (tolerance: float) (value: float) = abs (value) < tolerance

type LineSegment = (Point * Point)

let (|Horizontal|Vertical|SinglePoint|Other|) = function
    | ((x1, y1), (x2, y2)) when x1 = x2 && y1 = y2 -> SinglePoint
    | ((x1, _), (x2, _)) when x1 = x2 -> Horizontal
    | ((_, y1), (_, y2)) when y1 = y2 -> Vertical
    | _ -> Other

let midpoint ((x1, y1), (x2, y2)) = ((x1 + x2) / 2., (y1 + y2) / 2.)

let length ((x1, y1), (x2, y2)) =
    let dx = x2 - x1
    let dy = y2 - y1
    Math.Sqrt (dx * dx + dy * dy)

let extend factor ((x1, y1), (x2, y2)) =
    match (x1, y1), (x2, y2) with
    | Horizontal ->
        let x2' = (x2 - x1) * factor + x1
        (x1, y1), (x2', y1)
    | Vertical ->
        let y2' = (y2 - y1) * factor + y1
        (x1, y1), (x2, y2')
    | SinglePoint -> (x1, y1), (x1, y1)
    | Other ->
        let ratio = (x2 - x1) / (y2 - y1)
        let y2' = (y2 - y1) * factor + y1
        let x2' = (y2' - y1) * ratio + x1
        (x1, y1), (x2', y2')

let area2 (x1, y1) (x2, y2) (x3, y3): float =
    (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)

type LeftResult = Left | Collinear | Right

let left tolerance area2 =
    match area2 with
    | x when isZero tolerance x -> Collinear
    | x when x > 0. -> Left
    | x when x < 0. -> Right
    | _ -> invalidOp "bug: this should never happen"

let between (left: LeftResult) (x1, y1) (x2, y2) (x3, y3) =
    if left <> Collinear then false
    else
        // if p1-p2 is not vertical, check betweenness on X
        if x1 <> x2 then
            (x1 <= x3 && x3 <= x2) || (x1 >= x3 && x3 >= x2) 
        // else on Y
        else
            (y1 <= y3 && y3 <= y2) || (y1 >= y3 && y3 >= y2)

type LineSegmentsIntersectionResult =
    | Same
    | Opposite
    | NotIntersect
    | SharingOneVertex
    | OneVertexLiesOnOtherSegment
    | CollinearOverlapping
    | Intersect

let doLineSegmentsIntersect tolerance
    ((p1, p2): LineSegment) ((p3, p4): LineSegment) =

    let determineCollinearityStatus p1On p2On p1Collinear p2Collinear =
        match p1On, p2On, p1Collinear, p2Collinear with
        | (true, true, _, _) -> Some CollinearOverlapping
        | (true, false, _, false) -> Some OneVertexLiesOnOtherSegment
        | (true, false, _, true) -> Some CollinearOverlapping
        | (false, true, false, _) -> Some OneVertexLiesOnOtherSegment
        | (false, true, true, _) -> Some CollinearOverlapping
        | _ -> None
    
    let v13 = p1 = p3
    let v24 = p2 = p4
    let v14 = p1 = p4
    let v23 = p2 = p3
        
    if v13 && v24 then Same
    elif v14 && v23 then Opposite
    elif v13 || v24 || v14 || v23 then SharingOneVertex
    else
        let abc = area2 p1 p2 p3
        let abd = area2 p1 p2 p4
        let cda = area2 p3 p4 p1
        let cdb = area2 p3 p4 p2

        let abcLeft = left tolerance abc
        let abdLeft = left tolerance abd
        let cdaLeft = left tolerance cda
        let cdbLeft = left tolerance cdb

        let p3Collinear = abcLeft = Collinear
        let p4Collinear = abdLeft = Collinear
        let p1Collinear = cdaLeft = Collinear
        let p2Collinear = cdbLeft = Collinear
        
        let doIntersectProper =
            if p1Collinear || p2Collinear || p3Collinear || p4Collinear then
                false
            else
                (xor (abcLeft = Left) (abdLeft = Left))
                && (xor (cdaLeft = Left) (cdbLeft = Left))

        if doIntersectProper then Intersect
        else
            let p3On = between abcLeft p1 p2 p3
            let p4On = between abdLeft p1 p2 p4
            let p1On = between cdaLeft p3 p4 p1
            let p2On = between cdbLeft p3 p4 p2
            
            match
                (determineCollinearityStatus p3On p4On p3Collinear p4Collinear,
                    determineCollinearityStatus p1On p2On p1Collinear p2Collinear) with
            | (None, None) -> NotIntersect
            | (Some OneVertexLiesOnOtherSegment, None) -> OneVertexLiesOnOtherSegment
            | (Some CollinearOverlapping, _) -> CollinearOverlapping
            | (None, Some OneVertexLiesOnOtherSegment) -> OneVertexLiesOnOtherSegment
            | (_, Some CollinearOverlapping) -> CollinearOverlapping
            | _ -> invalidOp "bug: this should never happen"
        
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
//        let ((p1, p2), (p3, p4)) = (line1, line2)
        
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
            | _ -> true |> Prop.classify true "not covered yet"

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