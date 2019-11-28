module Projections.``Mercator tests``

open Demeton.Geometry.Common
open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Projections.PROJParsing

open System

open Xunit
open Projections.ProjectionsTestHelpers
open Swensen.Unquote
open TestHelp
       
[<Fact>]
let ``Parses PROJ specification that uses Mercator``() =
    let parseResult = parseProjSpecProjection "+proj=merc +lat_ts=56.5"
    test <@ parseResult
            |> isOkValue { Projection = Mercator;
                           IgnoredParameters = [
                               { Name = "lat_ts"; Value = NumericValue 56.5 }
                           ] } @>

[<Theory>]
[<InlineData(0., 0., 0., 0.)>]
[<InlineData(10., 10., 0.17453293, 0.17542583)>]
[<InlineData(10., 80., 0.17453293, 2.43624605)>]
[<InlineData(180., -80., 3.14159265, -2.43624605)>]
let ``Correctly projects`` longitude latitude expectedX expectedY =
    let mapScale = MapScale.ScaleOf1
    let projection = Factory.createMapProjection Mercator mapScale
    
    test <@ projection.Proj (degToRad longitude) (degToRad latitude)
        |> expectXY expectedX expectedY @>

[<Theory>]
[<InlineData(0., 86)>]
[<InlineData(0., -86)>]
let ``Returns None if latitude is outside of Mercator bounds``
    longitude latitude =

    let mapScale = MapScale.ScaleOf1
    let projection = Factory.createMapProjection Mercator mapScale
        
    projection.Proj (degToRad longitude) (degToRad latitude) |> expectNone

[<ProjectLonLat>]
let ``Mercator projection formulas are correct`` (lonLat: ProjectLonLat) =
    let mapScale = { MapScale = 100000.; Dpi = 300. }
    let projection = Factory.createMapProjection Mercator mapScale
    
    let (lonDegrees, latDegrees) = lonLat
    let lon = degToRad lonDegrees
    let lat = degToRad latDegrees
    let pointOption = projection.Proj lon lat
    match pointOption with
    | None -> 
        lat > Mercator.MinLat || lat < Mercator.MaxLat
    | Some (x, y) ->
        let inverse = projection.Invert x y
        match inverse with
        | None -> false
        | Some (lon', lat') ->
            Math.Round(lon', 10) |> isApproxEqualTo lon (Decimals 10)
            && Math.Round(lat', 10) |> isApproxEqualTo lat (Decimals 10)
