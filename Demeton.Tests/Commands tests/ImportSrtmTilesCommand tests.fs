module ``Commands tests``.``ImportSrtmTilesCommand tests``

open Demeton.Commands
open Demeton.Geometry
open Demeton.Srtm
open Demeton.Srtm.Types
open Demeton.DemTypes

open Xunit
open Swensen.Unquote
open TestHelp

let parseArgs = ImportSrtmTilesCommand.parseArgs

let isOkWithOptions result: ImportSrtmTilesCommand.Options =
    match result with
    | Ok options -> options
    | _ -> invalidOp "Expected the parsed options."


[<Fact>]
let ``Reports error when bounds parameter is missing``() =
    let result = parseArgs []
    test <@ result |> isErrorData "'bounds' parameter is missing." @>


[<Theory>]
[<InlineData("10,20")>]
[<InlineData("10,20,30")>]
[<InlineData("10,20,30,40,50")>]
let ``Reports error when bounds value is not made of 4 parts`` parameterValue =
    let result = parseArgs [ "--bounds"; parameterValue ]
    test <@ 
            result 
            |> isErrorData ("'bounds' option's value is invalid,"
                        + " it should consist of 4 numbers.")
    @>


[<Fact>]
let ``Reports error when at least one of bounds parts is not a number``() =
    let result = parseArgs [ "--bounds"; "10,20,a,30" ]
    test <@ 
            result 
            |> isErrorData ("'bounds' option's value is invalid, "
                        + "it should consist of 4 comma-separated numbers.") 
    @>


[<Theory>]
[<InlineData("-500,20,25,30")>]
[<InlineData("-50,20,2500,30")>]
let ``Reports error when longitude value is out of bounds`` parameterValue =
    let result = parseArgs [ "--bounds"; parameterValue ]
    test <@ 
            result 
            |> isErrorData ("'bounds' option's value is invalid, "
                        + "longitude value is out of range.") 
        @>


[<Theory>]
[<InlineData("-10,-100,25,30")>]
[<InlineData("-50,20,25,100")>]
let ``Reports error when latitude value is out of bounds`` parameterValue =
    let result = parseArgs [ "--bounds"; parameterValue ]
    test <@ 
            result 
            |> isErrorData ("'bounds' option's value is invalid, "
                        + "latitude value is out of range.") 
        @>


[<Fact>]
let ``Reports error when min and max longitude values are switched``() =
    let result = parseArgs [ "--bounds"; "80,10,70,30" ]
    test <@ 
            result 
            |> isErrorData ("'bounds' option's value is invalid, "
                + "max longitude value is smaller than min longitude value.") 
        @>


[<Fact>]
let ``Reports error when min and max latitude values are switched``() =
    let result = parseArgs [ "--bounds"; "10,20,25,-10" ]
    test <@ 
            result 
            |> isErrorData ("'bounds' option's value is invalid, "
                + "max latitude value is smaller than min latitude value.") 
        @>


[<Fact>]
let ``Parses valid bounds values``() =
    let result = parseArgs [ "--bounds"; "-10.1,20,25,70.4" ]
    test <@ 
            (result |> isOkWithOptions).Bounds = 
                Some { MinLon = -10.1;
                    MinLat = 20.;
                    MaxLon = 25.;
                    MaxLat = 70.4 }
        @>


[<Fact>]
let ``The default SRTM dir is 'srtm'``() =
    let result = parseArgs [ "--bounds"; "-10.1,20,25,70.4" ]
    test <@ 
            (result |> isOkWithOptions).SrtmDir = "srtm"
        @>


[<Fact>]
let ``Parses the SRTM dir parameter``() =
    let srtmDirValue = "somewhere/else"

    let result = 
        parseArgs [ 
        "--srtm-dir"; srtmDirValue
        "--bounds"; "-10.1,20,25,70.4";
        ]
    test <@ 
            (result |> isOkWithOptions).SrtmDir = srtmDirValue
        @>


[<Fact>]
let ``The default local cache dir is 'cache'``() =
    let result = parseArgs [ "--bounds"; "-10.1,20,25,70.4" ]
    test <@ 
            (result |> isOkWithOptions).LocalCacheDir = "cache"
        @>


[<Fact>]
let ``Parses the local cache dir parameter``() =
    let localCacheDirValue = "somewhere/else"

    let result = 
        parseArgs [ 
        "--local-cache-dir"; localCacheDirValue
        "--bounds"; "-10.1,20,25,70.4";
        ]
    test <@ 
            (result |> isOkWithOptions).LocalCacheDir = localCacheDirValue
        @>


[<Fact>]
let ``Imports all tiles within the specified boundaries``() =
    let tilesCoords = [|
        { Lon = SrtmLongitude.fromInt 15; Lat = SrtmLatitude.fromInt 45 }
        { Lon = SrtmLongitude.fromInt 16; Lat = SrtmLatitude.fromInt 46 }
    |]

    // we use a lock because the import function employs parallelization
    let threadsLock = "some lock"
    let mutable tilesRead = []
    let mutable heightsArraysProduced: HeightsArray list = []

    let checkCachingStatus (tilesCoords: SrtmTileCoords) = 
        Tile.CachingStatus.NotCached

    let readTile (tilesCoords: SrtmTileCoords): HeightsArrayResult =
        lock threadsLock (fun () -> tilesRead <- tilesCoords :: tilesRead)
        let heightsArray = 
            HeightsArray (
                0, 0, 10, 10, HeightsArrayInitializer1D (
                    fun _ -> DemHeightNone))
        lock threadsLock (
            fun () -> 
                heightsArraysProduced <- heightsArray :: heightsArraysProduced)
        Ok (Some heightsArray)

    ImportSrtmTilesCommand.run tilesCoords checkCachingStatus readTile

    test <@ heightsArraysProduced.Length = tilesCoords.Length @>
