module Demeton.Tests.Projections.``LCC tests``

open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Projections.PROJParsing

open Xunit
open Swensen.Unquote
open TestHelp
       
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
