module Projections.``WebMercator tests``

open Demeton.Geometry.Common
open Demeton.Projections

open System

open Xunit
open Projections.ProjectionsTestHelpers
open Swensen.Unquote
open TestHelp

[<Theory>]
[<InlineData(0., 0., 0., 0.)>]
[<InlineData(10., 10., 0.17453293, 0.17542583)>]
[<InlineData(10., 80., 0.17453293, 2.43624605)>]
[<InlineData(180., -80., 3.14159265, -2.43624605)>]
let ``Correctly projects`` longitude latitude expectedX expectedY =

    test <@ WebMercator.proj (degToRad longitude) (degToRad latitude)
        |> expectXY expectedX expectedY @>

[<Theory>]
[<InlineData(0., 86)>]
[<InlineData(0., -86)>]
let ``Returns None if latitude is outside of Web Mercator bounds``
    longitude latitude =
        
    WebMercator.proj (degToRad longitude) (degToRad latitude) |> expectNone

[<ProjectLonLat>]
let ``WebMercator projection formulas are correct`` (lonLat: ProjectLonLat) = 
    let (lonDegrees, latDegrees) = lonLat
    let lon = degToRad lonDegrees
    let lat = degToRad latDegrees
    let pointOption = WebMercator.proj lon lat
    match pointOption with
    | None -> 
        lat > WebMercator.MinLat || lat < WebMercator.MaxLat
    | Some (x, y) ->
        let inverse = WebMercator.inverse x y
        match inverse with
        | None -> false
        | Some (ilon, ilat) ->
            Math.Round(ilon, 10) |> isApproxEqualTo lon (Decimals 10)
            && Math.Round(ilat, 10) |> isApproxEqualTo lat (Decimals 10)
