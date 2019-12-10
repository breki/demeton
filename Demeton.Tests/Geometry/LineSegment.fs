module Demeton.Geometry.LineSegment

open Demeton.Geometry.Common
open System

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

let extend (factor: float) ((x1, y1), (x2, y2)) =
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

type LineSegmentsIntersectionDetectionResult =
    | Same
    | Opposite
    | NotIntersect
    | SharingOneEndpoint
    | OneEndpointLiesOnOtherSegment
    | CollinearOverlapping
    | OneOrBothAreZeroLength
    | Intersect

let doLineSegmentsIntersect tolerance
    ((p1, p2): LineSegment) ((p3, p4): LineSegment) =

    let determineCollinearityStatus p1On p2On p1Collinear p2Collinear =
        match p1On, p2On, p1Collinear, p2Collinear with
        | (true, true, _, _) -> Some CollinearOverlapping
        | (true, false, _, false) -> Some OneEndpointLiesOnOtherSegment
        | (true, false, _, true) -> Some CollinearOverlapping
        | (false, true, false, _) -> Some OneEndpointLiesOnOtherSegment
        | (false, true, true, _) -> Some CollinearOverlapping
        | _ -> None

    if (p1 = p2) || (p3 = p4) then OneOrBothAreZeroLength
    else
        let v13 = p1 = p3
        let v24 = p2 = p4
        let v14 = p1 = p4
        let v23 = p2 = p3
            
        if v13 && v24 then Same
        elif v14 && v23 then Opposite
        elif v13 || v24 || v14 || v23 then SharingOneEndpoint
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
                | (Some OneEndpointLiesOnOtherSegment, None) ->
                    OneEndpointLiesOnOtherSegment
                | (Some CollinearOverlapping, _) -> CollinearOverlapping
                | (None, Some OneEndpointLiesOnOtherSegment) ->
                    OneEndpointLiesOnOtherSegment
                | (_, Some CollinearOverlapping) -> CollinearOverlapping
                | _ -> invalidOp "bug: this should never happen"

type LineSegmentsIntersectionResult =
    | IntersectProperly of Point
    | SharingOneEndpoint of Point
    | OneEndpointLiesOnOtherSegment of Point
    | CollinearOverlapping of Point
    | OneOrBothAreZeroLength
    | DoNotIntersect

let findParallelLineSegmentsIntersection
    tolerance (p1, p2) (p3, p4) =
    
    if (p1 = p3 && p2 = p4) || (p1 = p4 && p2 = p3) then
        LineSegmentsIntersectionResult.CollinearOverlapping p1
    elif p1 = p3 || p2 = p3 then SharingOneEndpoint p3
    elif p1 = p4 || p2 = p4 then SharingOneEndpoint p4
    else
        let left = left tolerance (area2 p1 p2 p3)
        if left <> Collinear then DoNotIntersect
        elif between Collinear p1 p2 p3 then
            LineSegmentsIntersectionResult.CollinearOverlapping p3
        elif between Collinear p1 p2 p4 then
            LineSegmentsIntersectionResult.CollinearOverlapping p4
        elif between Collinear p3 p4 p1 then
            LineSegmentsIntersectionResult.CollinearOverlapping p1
        elif between Collinear p3 p4 p2 then
            LineSegmentsIntersectionResult.CollinearOverlapping p2
        else
            DoNotIntersect
    
type ValuesComparisonResult = Below | Equal | Above
    
let compareTo tolerance value1 value2 =
    if value2 < value1 - tolerance then Below
    elif value2 < value1 + tolerance then Equal
    else Above
    
type Value01Determinator = Is0Or1 | Between0And1 | Outside

let determineValue01Status tolerance value =
    match (value |> compareTo tolerance 0.),
        (value |> compareTo tolerance 1.) with
    | Equal, Below -> Is0Or1
    | Above, Equal -> Is0Or1
    | Below, Below -> Outside
    | Above, Above -> Outside
    | Above, Below -> Between0And1
    | _ -> invalidOp "bug: this should never happen"
    
let findLineSegmentsIntersection
    tolerance
    (((x1, y1), (x2, y2)): LineSegment)
    (((x3, y3), (x4, y4)): LineSegment)
    : LineSegmentsIntersectionResult =
    
    if (x1 = x2 && y1 = y2) || (x3 = x4 && y3 = y4) then OneOrBothAreZeroLength
    else
        let denom =
            x1 * (y4 - y3) + x2 * (y3 - y4)
            + x4 * (y2 - y1) + x3 * (y1 - y2)
        
        if denom |> isZero tolerance then
            findParallelLineSegmentsIntersection
                tolerance ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
        else
            let num1 = x1 * (y4 - y3) + x3 * (y1 - y4) + x4 * (y3 - y1)
            let num2 = -(x1 * (y3 - y2) + x2 * (y1 - y3) + x3 * (y2 - y1))

            let s = num1 / denom
            let t = num2 / denom

            let calcIntersectionPoint() =
                (x1 + s * (x2 - x1), y1 + s * (y2 - y1))
            
            match (determineValue01Status tolerance s),
                    (determineValue01Status tolerance t) with
            | Is0Or1, Is0Or1 -> SharingOneEndpoint (calcIntersectionPoint())
            | Is0Or1, Between0And1 ->
                OneEndpointLiesOnOtherSegment (calcIntersectionPoint())
            | Between0And1, Is0Or1 ->
                OneEndpointLiesOnOtherSegment (calcIntersectionPoint())
            | Between0And1, Between0And1 ->
                IntersectProperly (calcIntersectionPoint())
            | _ -> DoNotIntersect
