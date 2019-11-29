module Demeton.Tests.Projections.``LCC tests``

open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Projections.PROJParsing
open Demeton.Geometry.Common

open Xunit
open Swensen.Unquote
open FsCheck
open TestHelp
open PropertiesHelp
       
[<Fact>]
let ``Uses the right defaults for projection parameters``() =
    let parseResult = parseProjSpecProjection "+proj=lcc"
    
    let expectedPars: LambertConformalConic.Parameters =
        { X0 = 0.; Y0 = 0.;
          Lon0 = 0.; Lat0 = 0.;
          Lat1 = 0.; Lat2 = 0.;
          K0 = 1.
          Ellipsoid = GRS80 }
    
    test <@ parseResult
            |> isOkValue { Projection = LambertConformalConic expectedPars;
                           IgnoredParameters = [] } @>
       
[<Fact>]
let ``Correctly parses projection parameters``() =
    let parseResult = 
        parseProjSpecProjection 
            ("+proj=lcc +lon_0=123 +lat_0=10.2 +lat_1=-11 +lat_2=0.1 "     
                + "+x_0=-123 +y_0=44 +k_0=78. +ellps=wgs84")
    
    let expectedPars: LambertConformalConic.Parameters =
        { X0 = -123.; Y0 = 44.;
          Lon0 = 123.; Lat0 = 10.2;
          Lat1 = -11.; Lat2 = 0.1;
          K0 = 78.
          Ellipsoid = WGS84 }
    
    test <@ parseResult
            |> isOkValue { Projection = LambertConformalConic expectedPars;
                           IgnoredParameters = [] } @>
       
[<Fact>]
let ``Reports an error if one of numeric parameters has a string value``() =
    let parseResult = 
        parseProjSpecProjection 
            "+proj=lcc +lon_0=123 +lat_0=something"
    
    test <@ parseResult
            |> isErrorData (InvalidProjectionParameters 
                "PROJ parameter 'lat_0' must have a numeric value.") @>
       
[<Fact>]
let ``Lists any ignored parameters when parsing``() =
    let parseResult = 
        parseProjSpecProjection 
            "+proj=lcc +par1=value1 +lon_0=123 +par2=value2"
    
    let expectedPars: LambertConformalConic.Parameters =
        { X0 = 0.; Y0 = 0.;
          Lon0 = 123.; Lat0 = 0.;
          Lat1 = 0.; Lat2 = 0.;
          K0 = 1.
          Ellipsoid = GRS80 }
    
    test <@ parseResult
            |> isOkValue { Projection = LambertConformalConic expectedPars;
                           IgnoredParameters = [
                               {Name = "par2"; Value = Some (StringValue "value2")}
                               {Name = "par1"; Value = Some (StringValue "value1")}
                           ] } @>

[<Fact>]
let ``Sampling LCC functions``() =
    let parameters: LambertConformalConic.Parameters =
        { X0 = 0.; Y0 = 0.;
          Lon0 = 0.; Lat0 = 0.;
          Lat1 = degToRad 25.; Lat2 = degToRad 55.;
          K0 = 1.
          Ellipsoid = WGS84 }
        
    let mapScale = { MapScale = 100000.; Dpi = 300. }
        
    let projection =
        LambertConformalConic.MapProjection(parameters, mapScale).projection
        
    match projection.Proj (degToRad 0.) (degToRad 45.) with
    | Some (x, y) ->
        test <@ x = 0. && y |> isApproxEqualTo 5172085.54 (Decimals 2)  @>
    | None -> fail "The projected coordinates should not have been None"
    

let lccProperties(lonDeg, latDeg) =
    let parameters: LambertConformalConic.Parameters =
        { X0 = 0.; Y0 = 0.;
          Lon0 = 0.; Lat0 = 0.;
          Lat1 = degToRad 25.; Lat2 = degToRad 55.;
          K0 = 1.
          Ellipsoid = WGS84 }
        
    let mapScale = { MapScale = 100000.; Dpi = 300. }
        
    let projection =
        LambertConformalConic.MapProjection(parameters, mapScale).projection
        
    match projection.Proj (degToRad lonDeg) (degToRad latDeg) with
    | Some (x, y) ->
        match projection.Invert x y with
        | Some (lon', lat') ->
            (lonDeg = (radToDeg lon') && latDeg = (radToDeg lat'))
            |> Prop.label "Inverted coordinates should be equal to original ones"
        | None ->
            false
            |> Prop.label "Inverted coordinates should not be None"
    | None ->
        true
        |> Prop.classify true "proj = None" // todo: check the lon lat
         
[<Fact(Skip="todo: continue")>]
let ``Test LCC properties``() =
    let genLon = floatInRange -180 180
    let genLat = floatInRange -90 90
    
    let genCoords = Gen.zip genLon genLat
    
    genCoords
    |> Arb.fromGen
    |> Prop.forAll <| lccProperties
    |> Check.QuickThrowOnFailure
