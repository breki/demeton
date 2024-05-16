module ``Commands tests``.``ImportSrtmTilesCommand tests``

open CommandLine.Common
open Demeton.Commands
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.Srtm.Fetch

open Xunit
open Swensen.Unquote
open TestHelp

let parseArgs args =
    let result = parseParameters args ImportSrtmTilesCommand.supportedParameters

    match result with
    | Ok parsedParameters ->
        parsedParameters |> ImportSrtmTilesCommand.fillOptions |> Ok
    | Error error -> Error error

let isOkWithOptions result : ImportSrtmTilesCommand.Options =
    match result with
    | Ok options -> options
    | _ -> invalidOp "Expected the parsed options."


[<Fact>]
let ``Reports error when bounds parameter is missing`` () =
    let result = parseArgs []
    test <@ result |> isErrorData "<bounds> argument's value is missing." @>


[<Theory>]
[<InlineData("10,20")>]
[<InlineData("10,20,30")>]
[<InlineData("10,20,30,40,50")>]
let ``Reports error when bounds value is not made of 4 parts`` parameterValue =
    let result = parseArgs [ parameterValue ]

    test
        <@
            result
            |> isErrorData (
                "<bounds> argument's value is invalid,"
                + " it should consist of 4 numbers."
            )
        @>


[<Fact>]
let ``Reports error when at least one of bounds parts is not a number`` () =
    let result = parseArgs [ "10,20,a,30" ]

    test
        <@
            result
            |> isErrorData (
                "<bounds> argument's value is invalid, "
                + "it should consist of 4 comma-separated numbers."
            )
        @>


[<Theory>]
[<InlineData("-500,20,25,30")>]
[<InlineData("-50,20,2500,30")>]
let ``Reports error when longitude value is out of bounds`` parameterValue =
    let result = parseArgs [ parameterValue ]

    test
        <@
            result
            |> isErrorData (
                "<bounds> argument's value is invalid, "
                + "longitude value is out of range."
            )
        @>


[<Theory>]
[<InlineData("-10,-100,25,30")>]
[<InlineData("-50,20,25,100")>]
let ``Reports error when latitude value is out of bounds`` parameterValue =
    let result = parseArgs [ parameterValue ]

    test
        <@
            result
            |> isErrorData (
                "<bounds> argument's value is invalid, "
                + "latitude value is out of range."
            )
        @>


[<Fact>]
let ``Reports error when min and max longitude values are switched`` () =
    let result = parseArgs [ "80,10,70,30" ]

    test
        <@
            result
            |> isErrorData (
                "<bounds> argument's value is invalid, "
                + "max longitude value is smaller than min longitude value."
            )
        @>


[<Fact>]
let ``Reports error when min and max latitude values are switched`` () =
    let result = parseArgs [ "10,20,25,-10" ]

    test
        <@
            result
            |> isErrorData (
                "<bounds> argument's value is invalid, "
                + "max latitude value is smaller than min latitude value."
            )
        @>


[<Fact>]
let ``Parses valid bounds values`` () =
    let result = parseArgs [ "-10.1,20,25,70.4" ]

    test
        <@
            (result |> isOkWithOptions).Bounds = Some
                { MinLon = -10.1
                  MinLat = 20.
                  MaxLon = 25.
                  MaxLat = 70.4 }
        @>


[<Fact>]
let ``The default SRTM dir is 'srtm'`` () =
    let result = parseArgs [ "-10.1,20,25,70.4" ]
    test <@ (result |> isOkWithOptions).SrtmDir = "srtm" @>


[<Fact>]
let ``Parses the SRTM dir parameter`` () =
    let srtmDirValue = "somewhere/else"

    let result = parseArgs [ "-10.1,20,25,70.4"; "--srtm-dir"; srtmDirValue ]
    test <@ (result |> isOkWithOptions).SrtmDir = srtmDirValue @>


[<Fact>]
let ``The default local cache dir is 'cache'`` () =
    let result = parseArgs [ "-10.1,20,25,70.4" ]
    test <@ (result |> isOkWithOptions).LocalCacheDir = "cache" @>


[<Fact>]
let ``Parses the local cache dir parameter`` () =
    let localCacheDirValue = "somewhere/else"

    let result =
        parseArgs
            [ "-10.1,20,25,70.4"; "--local-cache-dir"; localCacheDirValue ]

    test <@ (result |> isOkWithOptions).LocalCacheDir = localCacheDirValue @>


[<Fact>]
let ``Imports all tiles within the specified boundaries`` () =
    let tiles = [| demTileId 0 15 45; demTileId 0 16 46 |]

    // we use a lock because the import function employs parallelization
    let threadsLock = "some lock"
    let mutable tilesRead = []
    let mutable heightsArraysProduced: HeightsArray list = []

    let determineTileStatus (tile: DemTileId) = SrtmTileStatus.NotCached

    let readTile (tile: DemTileId) : HeightsArrayMaybeResult =
        lock threadsLock (fun () -> tilesRead <- tiles :: tilesRead)

        let heightsArray =
            HeightsArray(
                0,
                0,
                10,
                10,
                HeightsArrayInitializer1D(fun _ -> DemHeightNone)
            )

        lock threadsLock (fun () ->
            heightsArraysProduced <- heightsArray :: heightsArraysProduced)

        Ok(Some heightsArray)

    ImportSrtmTilesCommand.run tiles determineTileStatus readTile

    test <@ heightsArraysProduced.Length = tiles.Length @>
