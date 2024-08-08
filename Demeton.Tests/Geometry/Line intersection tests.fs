module Tests.Geometry.``Line intersection tests``

open Demeton.Geometry.LineSegment
open Demeton.Geometry.LineSegmentsIntersection
open Demeton.Geometry.Funcs

open System

open Xunit
open FsCheck
open PropertiesHelp
open Swensen.Unquote

let lowTolerance = 0.
let defaultTolerance = 0.0001
let highTolerance = 0.01

let pointsAreEqual tolerance (x1, y1) (x2, y2) =
    (Math.Pow(x2 - x1, 2.)) + (Math.Pow(y2 - y1, 2.)) < (tolerance * tolerance)

let isOneOf tolerance points point =
    points |> Seq.exists (fun p -> pointsAreEqual tolerance p point)

let liesOnTheSameSideOf tolerance (p1, p2) (p3, p4) =
    let left1 = left tolerance (area2 p1 p2 p3)
    let left2 = left tolerance (area2 p1 p2 p4)

    match left1, left2 with
    | Left, Left -> true
    | Right, Right -> true
    | Collinear, Collinear -> true
    | _ -> false

let liesOnSegment tolerance (p1, p2) p3 =
    between (left tolerance (area2 p1 p2 p3)) p1 p2 p3

let veryClose closenessTolerance (x1, y1) (x2, y2) =
    let distance = ((x1, y1), (x2, y2)) |> length
    let maxDistance = abs x1 * closenessTolerance
    distance <= maxDistance

let areLineSegmentsSame line1 line2 = line1 = line2
let areLineSegmentsOpposite (p1, p2) (p3, p4) = p1 = p4 && p2 = p3

let sharingOneEndpoint tolerance line1 line2 =
    let (p1, p2), (p3, p4) = (line1, line2)

    (((veryClose tolerance p1 p3)
      || (veryClose tolerance p1 p4)
      || (veryClose tolerance p2 p3)
      || (veryClose tolerance p2 p4))
     && not (areLineSegmentsOpposite line1 line2)
     && not (areLineSegmentsSame line1 line2))

let oneSegmentEndpointIsOnOtherSegment tolerance (p1, p2) (p3, p4) =
    (p3 |> liesOnSegment tolerance (p1, p2))
    || (p4 |> liesOnSegment tolerance (p1, p2))
    || (p1 |> liesOnSegment tolerance (p3, p4))
    || (p2 |> liesOnSegment tolerance (p3, p4))

type LineIntersectionPropertyTest(output: Xunit.Abstractions.ITestOutputHelper)
    =

    let isCollinear p1 p2 p3 =
        left defaultTolerance (area2 p1 p2 p3) = Collinear

    let isCollinearOverlappingWith tolerance (p1, p2) (p3, p4) =
        isCollinear p1 p2 p3
        && isCollinear p1 p2 p4
        && ((liesOnSegment tolerance (p1, p2) p3)
            || (liesOnSegment tolerance (p1, p2) p4))

    let propDetection result condition explanation =
        condition
        |> Prop.classify true (sprintf "%A" result)
        |> Prop.label explanation

    let wrongOrNoIntersectionPoint () =
        false |> Prop.label "wrong (or no) intersection point"

    let lineSegmentIntersectionProperties minLineLength (line1, line2) =
        let intersectionResult =
            doLineSegmentsIntersect defaultTolerance line1 line2

        let intersectionPoint =
            findLineSegmentsIntersection defaultTolerance line1 line2

        let (p1, p2), (p3, p4) = (line1, line2)

        let ``returned result matches the situation`` =
            match intersectionResult with
            | LineSegmentsIntersectionDetectionResult.OneOrBothAreZeroLength ->
                propDetection
                    intersectionResult
                    (p1 = p2 || p3 = p4)
                    "one or both of segments have zero length"
            | Same ->
                propDetection
                    intersectionResult
                    (areLineSegmentsSame line1 line2)
                    "two line segments are not the same"
            | Opposite ->
                propDetection
                    intersectionResult
                    (areLineSegmentsOpposite line1 line2)
                    "two line segments are not opposites of each other"
            | LineSegmentsIntersectionDetectionResult.SharingOneEndpoint ->
                propDetection
                    intersectionResult
                    (sharingOneEndpoint highTolerance line1 line2)
                    "two line segments do not share any of the vertices"
            | LineSegmentsIntersectionDetectionResult.OneEndpointLiesOnOtherSegment ->
                propDetection
                    intersectionResult
                    (oneSegmentEndpointIsOnOtherSegment
                        highTolerance
                        line1
                        line2)
                    "none of the vertices lie on other line segment"
            | LineSegmentsIntersectionDetectionResult.CollinearOverlapping ->
                propDetection
                    intersectionResult
                    ((line1 |> isCollinearOverlappingWith highTolerance line2)
                     || (line2 |> isCollinearOverlappingWith highTolerance line1))
                    "two line segments are not collinear and overlapping"
            | NotIntersect ->
                propDetection
                    intersectionResult
                    ((line1 |> liesOnTheSameSideOf defaultTolerance line2)
                     || (line2 |> liesOnTheSameSideOf defaultTolerance line1))
                    "two line segments intersect"
            | LineSegmentsIntersectionDetectionResult.IntersectProperly ->
                true // can't really verify this, so we just assume its true
                |> Prop.classify true "Intersect"

        let ``intersection point matches the situation`` =
            match intersectionResult with
            | LineSegmentsIntersectionDetectionResult.OneOrBothAreZeroLength ->
                intersectionPoint = OneOrBothAreZeroLength
                |> Prop.label
                    "one or both of line segments have zero length, but the intersection point function did not detect that"
            | Same ->
                intersectionPoint = CollinearOverlapping p1
                |> Prop.label
                    "intersection point when two line segments are the same"
            | Opposite ->
                intersectionPoint = CollinearOverlapping p1
                |> Prop.label "two line segments are opposites of each other"
            | LineSegmentsIntersectionDetectionResult.SharingOneEndpoint ->
                match intersectionPoint with
                | SharingOneEndpoint point ->
                    (point |> isOneOf defaultTolerance [ p1; p2; p3; p4 ])
                    |> Prop.label
                        "intersection point is none of line segment vertices"
                | OneEndpointLiesOnOtherSegment point ->
                    (point |> isOneOf defaultTolerance [ p1; p2; p3; p4 ])
                    |> Prop.label
                        "intersection point is none of line segment vertices"
                | _ -> wrongOrNoIntersectionPoint ()
            | LineSegmentsIntersectionDetectionResult.OneEndpointLiesOnOtherSegment ->
                match intersectionPoint with
                | OneEndpointLiesOnOtherSegment point ->
                    (point |> isOneOf defaultTolerance [ p1; p2; p3; p4 ])
                    |> Prop.label
                        "intersection point is none of line segment vertices"
                | _ -> wrongOrNoIntersectionPoint ()
            | LineSegmentsIntersectionDetectionResult.CollinearOverlapping ->
                match intersectionPoint with
                | CollinearOverlapping point ->
                    ((point |> liesOnSegment highTolerance line1)
                     && (point |> liesOnSegment highTolerance line2))
                    |> Prop.label
                        "two line segments are collinear and overlapping"
                | _ -> wrongOrNoIntersectionPoint ()
            | NotIntersect ->
                intersectionPoint = DoNotIntersect
                |> Prop.label "two line segments do not intersect"
            | LineSegmentsIntersectionDetectionResult.IntersectProperly ->
                match intersectionPoint with
                | IntersectProperly point ->
                    ((point |> liesOnSegment highTolerance line1)
                     && (point |> liesOnSegment highTolerance line2))
                    |> Prop.label
                        "intersection point does not lie on line segments"
                | _ -> wrongOrNoIntersectionPoint ()

        let ``situation matches the returned result`` =
            match line1, line2 with
            | _ when (p1 = p2 || p3 = p4) ->
                intersectionResult = LineSegmentsIntersectionDetectionResult.OneOrBothAreZeroLength
                |> Prop.label
                    "one or both of line segments have zero length, but the intersection detection function did not detect that"
            | _ when areLineSegmentsSame line1 line2 ->
                intersectionResult = Same
                |> Prop.label
                    "two line segments are same, but the intersection function did not detect that"
            | _ when areLineSegmentsOpposite line1 line2 ->
                intersectionResult = Opposite
                |> Prop.label
                    "two line segments are opposite, but the intersection function did not detect that"
            | _ when sharingOneEndpoint 0. line1 line2 ->
                intersectionResult = LineSegmentsIntersectionDetectionResult.SharingOneEndpoint
                |> Prop.label
                    "two line segments share one of the vertices, but the intersection function did not detect that"
            | _ when
                ((line1 |> isCollinearOverlappingWith 0. line2)
                 || (line2 |> isCollinearOverlappingWith 0. line1))
                ->
                intersectionResult = LineSegmentsIntersectionDetectionResult.CollinearOverlapping
                |> Prop.label
                    "two line segments are collinear and overlapping, but the intersection function did not detect that"
            | _ when oneSegmentEndpointIsOnOtherSegment 0. line1 line2 ->
                intersectionResult = LineSegmentsIntersectionDetectionResult.OneEndpointLiesOnOtherSegment
                |> Prop.label
                    "one (and only one) line segment's endpoints lies on other line segment, but the intersection function did not detect that"
            | _ when
                ((line1 |> liesOnTheSameSideOf lowTolerance line2)
                 || (line2 |> liesOnTheSameSideOf lowTolerance line1))
                ->
                match intersectionResult with
                | NotIntersect -> true |> Prop.label ""
                | LineSegmentsIntersectionDetectionResult.OneEndpointLiesOnOtherSegment ->
                    (liesOnSegment highTolerance line1 p3
                     || liesOnSegment highTolerance line1 p4
                     || liesOnSegment highTolerance line2 p1
                     || liesOnSegment highTolerance line2 p2)
                    |> Prop.label "funny case 1"
                | LineSegmentsIntersectionDetectionResult.CollinearOverlapping ->
                    (liesOnSegment highTolerance line1 p3
                     || liesOnSegment highTolerance line1 p4
                     || liesOnSegment highTolerance line2 p1
                     || liesOnSegment highTolerance line2 p2)
                    |> Prop.label "funny case 2"
                | _ ->
                    false
                    |> Prop.label
                        "two line segments do not intersect, but the intersection function did not detect that"
            | _ -> true |> Prop.classify true "nothing to verify in this case"

        if
            (line1 |> length < minLineLength)
            || (line2 |> length < minLineLength)
        then
            true |> Prop.classify true "very short lines"
        else
            (``returned result matches the situation``
             .&. ``intersection point matches the situation``
             .&. ``situation matches the returned result``)
            |@ sprintf
                "intersection detection function returned %A"
                intersectionResult
            |@ sprintf
                "intersection point function returned %A"
                intersectionPoint

    let ``line intersection properties`` (minLineLength, ((p1, p2), (p3, p4))) =
        // try out some combinations of points and lines order
        lineSegmentIntersectionProperties minLineLength ((p1, p2), (p3, p4))
        .&. lineSegmentIntersectionProperties minLineLength ((p2, p1), (p3, p4))
        .&. lineSegmentIntersectionProperties minLineLength ((p1, p2), (p4, p3))
        .&. lineSegmentIntersectionProperties minLineLength ((p2, p1), (p4, p3))
        .&. lineSegmentIntersectionProperties minLineLength ((p3, p4), (p1, p2))

    let runPropertyTests genCoord minLineLength =
        let genPoint = Gen.zip genCoord genCoord
        let genLine = Gen.zip genPoint genPoint
        let genLinePair = Gen.zip genLine genLine

        let genZeroLengthCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), _), ((x3, y3), (x4, y4))) ->
                (((x1, y1), (x1, y1)), ((x3, y3), (x4, y4))))

        let genSameCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                (((x1, y1), (x2, y2)), ((x1, y1), (x2, y2))))

        let genOppositeCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                (((x1, y1), (x2, y2)), ((x2, y2), (x1, y1))))

        let genSharingEndpointCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), (_, (x4, y4))) ->
                (((x1, y1), (x2, y2)), ((x2, y2), (x4, y4))))

        let genOneEndpointCollinearCase =
            genLinePair
            |> Gen.map (fun (line1, (_, p4)) ->
                let pointOnSegment1 = line1 |> extend 0.75 |> snd
                (line1, (pointOnSegment1, p4)))

        let genCollinearOverlappingCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                let line1 = ((x1, y1), (x2, y2))

                let p3 = midpoint line1
                let line2 = extend 3. (p3, (x2, y2))

                (line1, line2))

        let genCollinearNonOverlappingCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), _) ->
                let line1 = ((x1, y1), (x2, y2))

                let lineExtended = line1 |> extend 3.
                let p3 = lineExtended |> midpoint
                let _, p4 = lineExtended

                (line1, (p3, p4)))

        let genParallelCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, y2)), ((x3, y3), _)) ->
                let x4 = x3 + (x2 - x1)
                let y4 = y3 + (y2 - y1)
                (((x1, y1), (x2, y1)), ((x3, y3), (x4, y4))))

        let genParallelHorizontalCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (x2, _)), ((x3, y3), (x4, _))) ->
                (((x1, y1), (x2, y1)), ((x3, y3), (x4, y3))))

        let genParallelVerticalCase =
            genLinePair
            |> Gen.map (fun (((x1, y1), (_, y2)), ((x3, y3), (_, y4))) ->
                (((x1, y1), (x1, y2)), ((x3, y3), (x3, y4))))

        let genRandomCase = genLinePair

        let genCase =
            Gen.frequency
                [ (1, genZeroLengthCase)
                  (1, genSameCase)
                  (1, genOppositeCase)
                  (2, genOneEndpointCollinearCase)
                  (2, genCollinearOverlappingCase)
                  (2, genCollinearNonOverlappingCase)
                  (2, genSharingEndpointCase)
                  (2, genParallelCase)
                  (2, genParallelHorizontalCase)
                  (2, genParallelVerticalCase)
                  (5, genRandomCase) ]

        let gen = Gen.zip (Gen.constant minLineLength) genCase

        ``line intersection properties``
        |> checkPropertyWithTestSize gen output 500 1000
    //        |> replayPropertyCheck gen output (1543704735,296680478)

    [<Fact>]
    //    [<Trait("Category", "slow")>]
    member this.``Test line intersection detection properties using small floats``
        ()
        =
        let genCoord = floatInRange -100 100
        runPropertyTests genCoord 0.01

    [<Fact>]
    //    [<Trait("Category", "slow")>]
    member this.``Test line intersection detection properties using dense integers``
        ()
        =
        let genCoord = Gen.choose (-10, 10) |> Gen.map float
        runPropertyTests genCoord 0.01

    [<Fact>]
    //    [<Trait("Category", "slow")>]
    member this.``Test line intersection detection properties using big floats``
        ()
        =
        let genCoord = floatInRange 1000000000 1000000000
        runPropertyTests genCoord 1.

[<Fact>]
let ``Rounding error in t in findLineSegmentsIntersection function`` () =
    let line1 = ((69.06, 48.87), (95.6, 57.5))
    let line2 = ((88.965, 55.3425), (27.5, 11.04))

    let intersectionResult =
        doLineSegmentsIntersect defaultTolerance line1 line2

    test
        <@
            intersectionResult = LineSegmentsIntersectionDetectionResult.OneEndpointLiesOnOtherSegment
        @>

    test
        <@
            match findLineSegmentsIntersection defaultTolerance line1 line2 with
            | OneEndpointLiesOnOtherSegment _ -> true
            | _ -> false
        @>
