module Tests.Geometry.``Line intersection tests``

open Demeton.Geometry.Common
open System

open Xunit
open FsCheck
open PropertiesHelp

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
    | SameLine
    | OppositeLine
    | NotIntersect
    | SharingOneVertex
    | OneVertexLiesOnOtherSegment
    | CollinearOverlapping
    | Intersect

let doLineSegmentsIntersect tolerance (p1, p2) (p3, p4) =
    let xor (a: bool) (b: bool) = (not a) <> (not b)

    let v13 = p1 = p3
    let v24 = p2 = p4
    let v14 = p1 = p4
    let v23 = p2 = p3
        
    if v13 && v24 then SameLine
    elif v14 && v23 then OppositeLine
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

        let doIntersectProper =
            if (abcLeft = Collinear) || (abdLeft = Collinear)
                || (cdaLeft = Collinear) || (cdbLeft = Collinear) then false
            else
                (xor (abcLeft = Left) (abdLeft = Left))
                && (xor (cdaLeft = Left) (cdbLeft = Left))

        if doIntersectProper then Intersect
        elif (between abcLeft p1 p2 p3) || (between abdLeft p1 p2 p4)
            || (between cdaLeft p3 p4 p1) || (between cdbLeft p3 p4 p2) then
            CollinearOverlapping
        else NotIntersect
    
type LineIntersectionTestCase =
    | Same of (LineSegment * LineSegment)
    | Opposite of (LineSegment * LineSegment)
    | SharingVertex of (LineSegment * LineSegment)
    | CollinearOverlapping of (LineSegment * LineSegment)
    | CollinearNonOverlapping 
    | CollinearHorizontal
    | CollinearVertical
    | Parallel
    | ParallelHorizontal of (LineSegment * LineSegment)
    | ParallelVertical of (LineSegment * LineSegment)
    | RandomIntersecting
    | RandomNonIntersecting
        
type LineIntersectionPropertyTest
    (output: Xunit.Abstractions.ITestOutputHelper) =

    let tolerance = 0.0001
            
    let expect caseName expectedResult (line1, line2) =
        let actualResult = doLineSegmentsIntersect tolerance line1 line2
                               
        actualResult = expectedResult
        |> Prop.classify true caseName
        |> Prop.label (sprintf "%s should be %A" caseName expectedResult)
        |@ sprintf
               "line1: %A, line2: %A, actual result: %A"
               line1 line2 actualResult
            
    let ``line intersection detection properties`` case =
        match case with
        | Same lines -> expect "same lines" SameLine lines
        | Opposite lines -> expect "opposite lines" OppositeLine lines
        | CollinearOverlapping lines ->
            expect "collinear overlapping lines"
                LineSegmentsIntersectionResult.CollinearOverlapping lines
        | SharingVertex lines ->
            expect "lines sharing one point" SharingOneVertex lines
        | ParallelHorizontal lines ->
            expect "parallel horizontal lines" NotIntersect lines
        | ParallelVertical lines ->
            expect "parallel vertical lines" NotIntersect lines
        | _ -> true |> Prop.classify true "not yet covered case"
 
    [<Fact>]
    member this.``Test line intersection detection properties``() =
        let genCoord = floatInRange 0 100
        let genPoint = Gen.zip genCoord genCoord
        let genLine = Gen.zip genPoint genPoint
        let genLinePair = Gen.zip genLine genLine
        
        let genSameCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                Same (((x1, y1), (x2, y2)), ((x1, y1), (x2, y2))) ) 
        
        let genOppositeCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                Opposite (((x1, y1), (x2, y2)), ((x2, y2), (x1, y1))) ) 
        
        let genCollinearOverlappingCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                let line1 = ((x1, y1), (x2, y2))
                
                let p3 = midpoint line1
                let line2 = extend 3. (p3, (x2, y2))
                
                CollinearOverlapping (line1, line2) ) 
        
        let genSharingPointCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), (_, (x4, y4))) ->
                SharingVertex (((x1, y1), (x2, y2)), ((x2, y2), (x4, y4))) ) 
        
        let genParallelHorizontalCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, _)), ((x3, y3), (x4, _))) ->
                ParallelHorizontal (((x1, y1), (x2, y1)), ((x3, y3), (x4, y3))) ) 
        
        let genParallelVerticalCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (_, y2)), ((x3, y3), (_, y4))) ->
                ParallelVertical (((x1, y1), (x1, y2)), ((x3, y3), (x3, y4))) ) 
        
        let gen = Gen.frequency [
            (1, genSameCase) 
            (1, genOppositeCase) 
            (1, genCollinearOverlappingCase) 
            (1, genSharingPointCase) 
            (1, genParallelHorizontalCase) 
            (1, genParallelVerticalCase)
        ]
        
        ``line intersection detection properties``
        |> checkProperty gen output 
//        |> replayPropertyCheck gen output (708668550,296679696)