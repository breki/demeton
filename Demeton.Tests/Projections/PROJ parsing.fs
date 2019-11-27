module Tests.Projections.``PROJ parsing``

open Demeton.Projections.Parsing

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
let ``Reports an error if PROJ parameter does not start with + (case 1)``() =
    let result = parseProjSpecParameters "proj=merc"
    
    test <@ result |> isErrorData 
                { Message = "Expected: parameter indicator '+'";
                  Location = 0 } @>

[<Fact>]
let ``Reports an error if PROJ parameter does not start with + (case 2)``() =
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
