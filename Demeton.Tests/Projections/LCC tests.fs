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

// used https://mygeodata.cloud/cs2cs/ as a source for control data
// from: +proj=longlat +datum=WGS84 +no_defs
// to: +proj=lcc +ellps=WGS84 +lat_1=25 +lat_2=55
[<Theory>]
[<InlineData(0.,15.,25.,55.,0.,1872030.82778)>]
[<InlineData(0.,45.,25.,55.,0.,5172085.53971)>]
[<InlineData(10.,45.,25.,55.,762227.613999,5215404.28979)>]
[<InlineData(0.,60.,25.,55.,0.,6828656.81024)>]
let ``Sampling LCC functions``(lon, lat, lat1, lat2, expectedX, expectedY) =
    let parameters: LambertConformalConic.Parameters =
        { X0 = 0.; Y0 = 0.;
          Lon0 = 0.; Lat0 = 0.;
          Lat1 = degToRad lat1; Lat2 = degToRad lat2;
          K0 = 1.
          Ellipsoid = WGS84 }
        
    let mapScale = { MapScale = 100000.; Dpi = 300. }
        
    let projection =
        LambertConformalConic.MapProjection(parameters, mapScale).projection
        
    match projection.Proj (degToRad lon) (degToRad lat) with
    | Some (x, y) ->
        test <@ x |> isApproxEqualTo expectedX (Decimals 2) @>
        test <@ y |> isApproxEqualTo expectedY (Decimals 2) @>
        
        match projection.Invert x y with
        | Some (lon', lat') ->
            test <@ (radToDeg lon') |> isApproxEqualTo lon (Decimals 6) @>
            test <@ (radToDeg lat') |> isApproxEqualTo lat (Decimals 6) @>
        | None -> fail "The inverted coordinates should not have been None"

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
            ((radToDeg lon') |> isApproxEqualTo lonDeg (Decimals 6)
             && (radToDeg lat') |> isApproxEqualTo latDeg (Decimals 6))
            |> Prop.label "Inverted coordinates should be equal to original ones"
        | None ->
            false
            |> Prop.label "Inverted coordinates should not be None"
    | None ->
        true
        |> Prop.classify true "proj = None" // todo: check the lon lat
         
[<Fact>]
let ``Test LCC properties``() =
    let genLon = floatInRange -180 180
    let genLat = floatInRange -90 90
    
    let genCoords = Gen.zip genLon genLat
    
    genCoords
    |> Arb.fromGen
    |> Prop.forAll <| lccProperties
    |> Check.QuickThrowOnFailure
