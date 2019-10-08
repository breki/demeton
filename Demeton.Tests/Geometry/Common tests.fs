module Geometry.``Common tests``

open Demeton.Geometry.Common

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit
open PropertiesHelp

[<Fact>]
let ``Splits coords into two lists``() =
    test <@ 
            [ 10.; 20.; 30.; 40. ] |> splitCoords = ([10.; 30.], [20.; 40.])
         @>

[<Fact>]
let ``Constructs a list of points from a coords list``() =
    test <@ 
            [ 10.; 20.; 30.; 40. ] |> floatsListToPoints 
                = ([(10., 20.); (30., 40.)])
         @>

[<Fact>]
let ``Returns Ok if all points are inside the box``() =
    let points = 
        [(10., 20.); (11., 21.); (12., 22.)]

    test <@ areAllPointsInsideBox 5. 7. 50. 60. points @>


[<Theory>]
[<InlineData(100., 20.)>]
[<InlineData(15., 100.)>]
[<InlineData(15., -100.)>]
[<InlineData(-100, 20.)>]
let ``Returns an error if some points are outside of the box``
    (offendingX, offendingY) =
    let points = 
        [(10., 20.); (11., 21.); (offendingX, offendingY); (12., 22.)]

    test <@ not (areAllPointsInsideBox 5. 7. 50. 60. points) @>

[<Property>]
let ``Normalizes the angle``() =
    let normalizer = 360.

    let isPositive angle = 
        normalizer |> normalizeAngle angle >= 0. |@ sprintf "isPositive"
    let isBelowNormalizer angle = 
        normalizer |> normalizeAngle angle < normalizer 
        |@ sprintf "is below normalizer"
    let isSameRemainderWhenPositive angle = 
        angle >= 0. ==> lazy
            angle % normalizer 
                .=. (normalizer |> normalizeAngle angle) % normalizer 
            |@ sprintf "is same remainder"
    let distanceBetweenAngleAndItsNormalizedValueIsMultiplierOfNormalizer 
        angle =
            let distance = angle - (normalizer |> normalizeAngle angle)
            distance % normalizer = 0. 
            |@ sprintf "distance is multiplier of normalizer"

    let props x = 
        (isPositive x) .&. (isBelowNormalizer x) 
        .&. (isSameRemainderWhenPositive x)
        .&. (distanceBetweenAngleAndItsNormalizedValueIsMultiplierOfNormalizer x)

    let genAngle = floatInRangeExclusive -1000 1000
    genAngle |> Arb.fromGen |> Prop.forAll <| props

[<Property>]
let ``Difference between angles``() =
    let normalizer = 360.

    let isPositive (angle1, angle2) = 
        normalizer |> differenceBetweenAngles angle1 angle2 >= 0.
    let isNeverMoreThanHalfOfNormalizer (angle1, angle2) = 
        normalizer |> differenceBetweenAngles angle1 angle2 <= (normalizer / 2.)
    let isCommutative (angle1, angle2) =
        normalizer |> differenceBetweenAngles angle1 angle2
            = (normalizer |> differenceBetweenAngles angle2 angle1)

    let props x = 
        (isPositive x) .&. (isNeverMoreThanHalfOfNormalizer x)
        .&. (isCommutative x)

    let genAngle1 = floatInRangeExclusive -1000 1000
    let genAngle2 = floatInRangeExclusive -1000 1000
    Gen.map2 (fun x y -> x, y) genAngle1 genAngle2
    |> Arb.fromGen |> Prop.forAll <| props