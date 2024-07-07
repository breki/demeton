module Demeton.Tests.Commands_tests.DemWithWaterBodiesCommand.Command_line_parsing

open CommandLine.Common
open Demeton.Commands
open Demeton.Projections.Common
open Demeton.Projections.PROJParsing
open Demeton.Shaders.Pipeline.Common

open Xunit
open Swensen.Unquote
open TestHelp


let parseArgs args =
    let result = parseParameters args DemWithWaterBodiesCommand.supportedParameters

    match result with
    | Ok parsedParameters -> parsedParameters |> DemWithWaterBodiesCommand.fillOptions |> Ok
    | Error error -> Error error


let getOptions result : DemWithWaterBodiesCommand.Options =
    match result with
    | Ok options -> options
    | _ -> invalidOp "Expected the parsed options."

[<Fact>]
let ``Can parse tile ID`` () =
    let result = parseArgs [ "N45E026" ]
    let options = getOptions result

    test
        <@
            options.TileId = { Lon = { Value = 26 }
                               Lat = { Value = 45 } }
        @>
