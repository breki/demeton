module Demeton.Tests.Commands_tests.DemWithWaterBodiesCommand.Command_line_parsing

open CommandLine.Common
open Demeton.Commands
open Demeton.Dem.Funcs

open Xunit
open Swensen.Unquote


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

    test <@ options.TileId = demTileXYId 26 45 @>
