module Tests.Shaders.Hillshading.``Calculating aspect and slope``

open Demeton.Geometry.Common
open Demeton.Projections.Common
open Demeton.Shaders
open System
open Xunit
open Swensen.Unquote
open TestHelp

let slopeAndAspectForHeights heights =
    let lon = degToRad 16.
    let lat = degToRad 45.
    // 500 meters in radians
    let dx = 500. / (cos lat * EarthRadiusInMeters)

    let coords = [|
        Some (lon-dx, lat+dx)
        Some (lon, lat+dx)
        Some (lon+dx, lat+dx)
        Some (lon-dx, lat)
        Some (lon, lat)
        Some (lon+dx, lat)
        Some (lon-dx, lat-dx)
        Some (lon, lat-dx)
        Some (lon+dx, lat-dx)
    |]

    match Hillshading.calculatePQ coords (Some heights) with
    | Some (p, q) -> 
        Hillshading.calculateSlopeAndAspect p q
    | _ -> invalidOp "bug"

[<Fact>]
let ``North facing face should have aspect of 0 degrees``() =
    let heights = [|
        Some 0.; Some 0.; Some 0.
        Some 500.; Some 500.; Some 500.
        Some 1000.; Some 1000.; Some 1000.
    |]

    let (slope, aspect) = slopeAndAspectForHeights heights
    test <@ aspect = 0. @>
    test <@ radToDeg slope |> isApproxEqualTo 35.26438968 (Decimals 6) @>

[<Fact>]
let ``East facing face should have aspect of 90 degrees``() =
    let heights = [|
        Some 1000.; Some 500.; Some 0.
        Some 1000.; Some 500.; Some 0.
        Some 1000.; Some 500.; Some 0.
    |]

    let (slope, aspect) = slopeAndAspectForHeights heights
    test <@ aspect = degToRad 90. @>
    test <@ radToDeg slope |> isApproxEqualTo 45.00317994 (Decimals 6) @>

[<Fact>]
let ``South facing face should have aspect of 90 degrees``() =
    let heights = [|
        Some 1000.; Some 1000.; Some 1000.
        Some 500.; Some 500.; Some 500.
        Some 0.; Some 0.; Some 0.
    |]

    let (slope, aspect) = slopeAndAspectForHeights heights
    test <@ normalizeAngle aspect (Math.PI * 2.) |> radToDeg = 180. @>
    test <@ radToDeg slope |> isApproxEqualTo 35.26438968 (Decimals 6) @>

[<Fact>]
let ``West facing face should have aspect of 90 degrees``() =
    let heights = [|
        Some 0.; Some 500.; Some 1000.
        Some 0.; Some 500.; Some 1000.
        Some 0.; Some 500.; Some 1000.
    |]

    let (slope, aspect) = slopeAndAspectForHeights heights
    test <@ normalizeAngle aspect (Math.PI * 2.) |> radToDeg = 270. @>
    test <@ radToDeg slope |> isApproxEqualTo 45.00317994 (Decimals 6) @>
