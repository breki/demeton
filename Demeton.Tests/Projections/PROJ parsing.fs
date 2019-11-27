module Tests.Projections.``PROJ parsing``

open Demeton.Projections.Parsing
open Demeton.Projections.Factory
open Demeton.Geometry.Common

open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
let ``Successfully parses PROJ specification into parameters list``() =
    let result = parseProjSpecParameters "+proj=merc +lat_ts=56.5"
    
    test <@ result |> isOkValue [
                { Name = "proj"; Value = StringValue "merc" };    
                { Name = "lat_ts"; Value = NumericValue 56.5 }    
                ]  @>

[<Fact>]
let ``Reports an error if PROJ parameter does not start with +``() =
    let result = parseProjSpecParameters "+proj=merc lat_ts=56.5"
    
    test <@ result |> isErrorData 
                { Message = "Expected: parameter indicator '+'";
                  Location = 11 } @>

[<Fact>]
let ``Reports an error if PROJ parameter name is missing``() =
    let result = parseProjSpecParameters "+=merc +lat_ts=56.5"
    
    test <@ result |> isErrorData 
                { Message = "Expected: parameter name";
                  Location = 1 } @>

[<Fact>]
let ``Reports an error if PROJ parameter does not continue with =``() =
    let result = parseProjSpecParameters "+proj +lat_ts=56.5"
    
    test <@ result |> isErrorData 
                { Message = "Expected: parameter value assignment '='";
                  Location = 5 } @>

[<Fact>]
let ``Reports an error if PROJ parameter does not have a value assigned``() =
    let result = parseProjSpecParameters "+proj= +lat_ts=56.5"
    
    test <@ result |> isErrorData 
                { Message = "Expected: parameter value";
                  Location = 6 } @>

       
[<Fact>]
let ``Parses PROJ specification that uses Mercator``() =
    let parseResult = parseProjSpecProjection "+proj=merc +lat_ts=56.5"
    test <@ parseResult
            |> isOkValue { Projection = Mercator;
                           IgnoredParameters = [
                               { Name = "lat_ts"; Value = NumericValue 56.5 }
                           ] } @>
       
[<Fact>]
let ``Reports an error if projection name is unsupported``() =
    let parseResult = parseProjSpecProjection "+proj=tmerc +lat_ts=56.5"
    test <@ parseResult |> isErrorData (UnsupportedProjection "tmerc") @>
       
[<Fact>]
let ``Reports an error if projection was not specified``() =
    let parseResult = parseProjSpecProjection "+something=tmerc +lat_ts=56.5"
    test <@ parseResult |> isErrorData ProjectionNotSpecified @>
       
[<Fact>]
let ``Reports an error if PROJ specification has a syntax error``() =
    let parseResult = parseProjSpecProjection "+proj=tmerc lat_ts=56.5"
    test <@ parseResult
            |> isErrorData (SpecParsingError {
                  Message = "Expected: parameter indicator '+'";
                  Location = 12 }) @>

[<Fact>]
let ``Maps Mercator to its projection functions``() =
    
    let projection = prepareProjectionFunctions Mercator
    
    let testLon = degToRad 15.
    let testLat = degToRad 46.
    test <@
         projection.Proj testLon testLat =
             Demeton.Projections.Mercator.proj testLon testLat @>
    test <@
             match projection.Proj testLon testLat with
             | Some (x, y) ->
                 match projection.Invert x y with
                 | Some (lon, lat) ->
                     (lon |> isApproxEqualTo testLon (Decimals 6))
                     && (lat |> isApproxEqualTo testLat (Decimals 6))
                 | None ->
                     fail "The invert function should have returned a lon/lat point."
             | None ->
                 fail "The proj function should have returned a point."
         @>
