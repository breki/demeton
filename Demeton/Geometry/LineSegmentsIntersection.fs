/// Contains functions for determining whether two line segments intersect
/// and calculating the point of intersection.
module Demeton.Geometry.LineSegmentsIntersection

open Demeton.Geometry.Common
open Demeton.Geometry.Funcs
open TolerantMath

/// Enumeration containing the possible results of the doLineSegmentsIntersect
/// function. 
type LineSegmentsIntersectionDetectionResult =
    /// The two line segments are identical.
    | Same
    /// The two line segments are opposites.
    | Opposite
    /// The two line segments do not intersect.
    | NotIntersect
    /// The two line segments share one endpoint (they are "connected").
    | SharingOneEndpoint
    /// One of the endpoints of one line segment lies on the other line segment
    /// (but not at its endpoints).
    | OneEndpointLiesOnOtherSegment
    /// The two line segments are collinear and overlapping.
    | CollinearOverlapping
    /// One or both of the line segments are of zero length.
    | OneOrBothAreZeroLength
    /// The two line segments do intersect "properly", i.e. not at one of their
    /// endpoints.
    | IntersectProperly

/// Determines whether the two specified line segments intersect and in what
/// way.
/// The basic algorithm checks whether each of the line segments has the other
/// segment's endpoints at different sides (one left and one right). If this is
/// true, we know the two segments intersect.
/// The function was adapted from Joseph O'Rourke's
/// "Computational Geometry in C" book (p. 29-33). 
let doLineSegmentsIntersect tolerance
    ((p1, p2): LineSegment) ((p3, p4): LineSegment) =

    let xor (a: bool) (b: bool) = (not a) <> (not b)
    
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

            if doIntersectProper then IntersectProperly
            else
                let p3On = between abcLeft p1 p2 p3
                let p4On = between abdLeft p1 p2 p4
                let p1On = between cdaLeft p3 p4 p1
                let p2On = between cdbLeft p3 p4 p2
                
                let p34Collinearity =
                    determineCollinearityStatus
                        p3On p4On p3Collinear p4Collinear
                let p12Collinearity =
                    determineCollinearityStatus
                        p1On p2On p1Collinear p2Collinear
                match p34Collinearity, p12Collinearity with
                | (None, None) -> NotIntersect
                | (Some OneEndpointLiesOnOtherSegment, None) ->
                    OneEndpointLiesOnOtherSegment
                | (Some CollinearOverlapping, _) -> CollinearOverlapping
                | (None, Some OneEndpointLiesOnOtherSegment) ->
                    OneEndpointLiesOnOtherSegment
                | (_, Some CollinearOverlapping) -> CollinearOverlapping
                | _ -> invalidOp "bug: this should never happen"

/// Possible results of the findLineSegmentsIntersection function. 
type LineSegmentsIntersectionResult =
    /// The two line segments do intersect "properly", i.e. not at one of their
    /// endpoints.
    | IntersectProperly of Point
    /// The two line segments share one endpoint (they are "connected").
    | SharingOneEndpoint of Point
    /// One of the endpoints of one line segment lies on the other line segment
    /// (but not at its endpoints).
    | OneEndpointLiesOnOtherSegment of Point
    /// The two line segments are collinear and overlapping.
    | CollinearOverlapping of Point
    /// One or both of the line segments are of zero length.
    | OneOrBothAreZeroLength
    /// The two line segments do not intersect.
    | DoNotIntersect

/// Calculates the intersection point (if any) of the two line segments when
/// they are parallel. This function is used by findLineSegmentsIntersection
/// function to handle the special case of parallel line segments.
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
    
/// Calculates the intersection point (if any) of the two line segments.
/// The function was adapted from Joseph O'Rourke's
/// "Computational Geometry in C" book (p. 220-226). 
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
