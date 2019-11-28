module Demeton.Tests.Projections.``LCC tests``

open Demeton.Projections
open Demeton.Projections.Common
open Demeton.Projections.PROJParsing

open Xunit
open Swensen.Unquote
open TestHelp
       
[<Fact(Skip="todo continue with the test")>]
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
