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

    let allProperties x = 
        (isPositive x) .&. (isBelowNormalizer x) 
        .&. (isSameRemainderWhenPositive x)
        .&. (distanceBetweenAngleAndItsNormalizedValueIsMultiplierOfNormalizer x)

    let genAngle = floatInRangeExclusive -1000 1000
    genAngle |> Arb.fromGen |> Prop.forAll <| allProperties
