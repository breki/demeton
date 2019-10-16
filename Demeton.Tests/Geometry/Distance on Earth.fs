module Tests.Geometry.``Distance on Earth``

open Demeton.Geometry.Common
open Demeton.Projections.Common

open System

open Xunit
open FsCheck
open Swensen.Unquote
open PropertiesHelp
open TestHelp

let ``is always non-negative`` ((lon1, lat1), (lon2, lat2)) =
    let distance = geodeticDistanceApproximate lon1 lat1 lon2 lat2
    distance > 0. |> Prop.label "is always non-negative"

let ``is never larger than half the Earth's circumference`` 
    ((lon1, lat1), (lon2, lat2)) =
    let earthCircumferenceHalf = EarthRadiusInMeters * Math.PI

    let distance = geodeticDistanceApproximate lon1 lat1 lon2 lat2
    distance < earthCircumferenceHalf 
    |> Prop.label "is never larger than half the Earth's circumference"

let ``is commutative`` ((lon1, lat1), (lon2, lat2)) =
    let distance1 = geodeticDistanceApproximate lon1 lat1 lon2 lat2
    let distance2 = geodeticDistanceApproximate lon2 lat2 lon1 lat1 
    distance1 = distance2 |> Prop.label "is commutative"

[<Fact>]
let ``geodeticDistanceApproximate properties``() =
    let genLongitude = floatInRange 0 360 |> Gen.map degToRad
    let genLatitude = floatInRange -90 90 |> Gen.map degToRad

    let genPoint = Gen.zip genLongitude genLatitude
    let genPair = Gen.zip genPoint genPoint

    let spec x = 
        (``is always non-negative`` x)
        .&. (``is never larger than half the Earth's circumference`` x)
        .&. (``is commutative`` x)

    genPair |> Arb.fromGen
    |> Prop.forAll <| spec
    |> Check.QuickThrowOnFailure

[<Theory>]
[<InlineData(15.6455, 46.557611, 14.508333, 46.055556, 104.08)>]
[<InlineData(15.6455, 46.557611, 15.983333, 45.816667, 86.43)>]
[<InlineData(15.6455, 46.557611, -0.1275, 51.507222, 1271.04)>]
let ``Control values`` (lon1, lat1, lon2, lat2, expectedDistanceInKm) =
    test <@ 
            let actualDistance =
                geodeticDistanceApproximate 
                    (degToRad lon1) (degToRad lat1) 
                    (degToRad lon2) (degToRad lat2)

            let expectedDistance = expectedDistanceInKm * 1000.
            actualDistance |> isApproxEqualTo expectedDistance (Percentage 10.) 
    @>
