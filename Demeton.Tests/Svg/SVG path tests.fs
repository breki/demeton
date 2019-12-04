module Demeton.Tests.Svg.``SVG path tests``

open Svg.Paths

open Xunit
open Swensen.Unquote

[<Fact>]
let ``Can serialize path data to string``() =
    let curveToParameters = [
        { StartControlPoint = (10.2345,22.);
          EndControlPoint = (-33.2, 44.2); Point = (12., 44.) }
        { StartControlPoint = (12.2,22.);
          EndControlPoint = (-34.2, 44.2); Point = (15., 44.) }
    ]
    
    let ellipticalArcParameters = [
        { Rx = 10.2; Ry = 55.3; XAxisRotation = 55.4; Arc = LargeArc;
          Sweep = PositiveAngle; Point = (-22.4, 8.) }
        { Rx = 11.2; Ry = 55.3; XAxisRotation = -55.4; Arc = SmallArc;
          Sweep = NegativeAngle; Point = (22.4, 8.) }
    ]
    
    let coords = [ 10.2; 12.2; -4.3 ]
    let points = [ (1.2, 3.4); (5.6, 7.8) ]
    
    let quadCurveToParameters = [
        { StartControlPoint = (10.2,22.); Point = (12., 44.) }
        { StartControlPoint = (12.2,22.); Point = (15., 44.) }
    ]
    
    let smoothCurveToParameters = [
        { EndControlPoint = (10.2,22.); Point = (12., 44.) }
        { EndControlPoint = (12.2,22.); Point = (15., 44.) }
    ]
    
    let pathData = [
        ClosePath
        CurveToAbs curveToParameters
        CurveToRel curveToParameters
        EllipticalArcAbs ellipticalArcParameters
        EllipticalArcRel ellipticalArcParameters
        HorizLineToAbs coords
        HorizLineToRel coords
        LineToAbs points
        LineToRel points
        MoveToAbs points
        MoveToRel points
        QuadraticCurveToAbs quadCurveToParameters
        QuadraticCurveToRel quadCurveToParameters
        SmoothCurveToAbs smoothCurveToParameters
        SmoothCurveToRel smoothCurveToParameters
        SmoothQuadraticCurveToAbs points
        SmoothQuadraticCurveToRel points
        VertLineToAbs coords
        VertLineToRel coords
    ]
    
    let expectedString = 
        "Z"
        + "C10.23 22-33.2 44.2 12 44 12.2 22-34.2 44.2 15 44"
        + "c10.23 22-33.2 44.2 12 44 12.2 22-34.2 44.2 15 44"
        + "A10.2 55.3 55.4 1 1-22.4 8 11.2 55.3-55.4 0 0 22.4 8"
        + "a10.2 55.3 55.4 1 1-22.4 8 11.2 55.3-55.4 0 0 22.4 8"
        + "H10.2 12.2-4.3"
        + "h10.2 12.2-4.3"
        + "L1.2 3.4 5.6 7.8"
        + "l1.2 3.4 5.6 7.8"
        + "M1.2 3.4 5.6 7.8"
        + "m1.2 3.4 5.6 7.8"
        + "Q10.2 22 12 44 12.2 22 15 44"
        + "q10.2 22 12 44 12.2 22 15 44"
        + "S10.2 22 12 44 12.2 22 15 44"
        + "s10.2 22 12 44 12.2 22 15 44"
        + "T1.2 3.4 5.6 7.8"
        + "t1.2 3.4 5.6 7.8"
        + "V10.2 12.2-4.3"
        + "v10.2 12.2-4.3"
    
    test <@ pathDataToString pathData = expectedString @>
