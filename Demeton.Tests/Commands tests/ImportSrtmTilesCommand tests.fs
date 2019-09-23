module Demeton.Tests.``Commands tests``.``ImportSrtmTilesCommand tests``

open Demeton.Commands.ImportSrtmTilesCommand
open Demeton.GeometryTypes
open Demeton.SrtmTypes
open Demeton.DemTypes

open Xunit
open Swensen.Unquote
open System.IO


let isError errorMessage result =
    match result with
    | Ok _ -> false
    | Error actualMessage -> actualMessage = errorMessage


let parsedOptions result: ImportOptions =
    match result with
    | Ok (_, options) -> options
    | _ -> invalidOp "Expected the parsed options."


[<Fact>]
let ``Reports error when bounds parameter is missing``() =
    let result = parseImportArgs []
    test <@ result |> isError "'bounds' parameter is missing." @>


[<Fact>]
let ``Reports error when bounds value is missing``() =
    let result = parseImportArgs [ "--bounds" ]
    test <@ result |> isError "'bounds' parameter's value is missing." @>


[<Theory>]
[<InlineData("10,20")>]
[<InlineData("10,20,30")>]
[<InlineData("10,20,30,40,50")>]
let ``Reports error when bounds value is not made of 4 parts`` parameterValue =
    let result = parseImportArgs [ "--bounds"; parameterValue ]
    test <@ 
            result 
            |> isError ("'bounds' parameter's value is invalid,"
                        + " it should consist of 4 numbers.")
    @>


[<Fact>]
let ``Reports error when at least one of bounds parts is not a number``() =
    let result = parseImportArgs [ "--bounds"; "10,20,a,30" ]
    test <@ 
            result 
            |> isError ("'bounds' parameter's value is invalid, "
                        + "it should consist of numbers only.") 
    @>


[<Theory>]
[<InlineData("-500,20,25,30")>]
[<InlineData("-50,20,2500,30")>]
let ``Reports error when longitude value is out of bounds`` parameterValue =
    let result = parseImportArgs [ "--bounds"; parameterValue ]
    test <@ 
            result 
            |> isError ("'bounds' parameter's value is invalid, "
                        + "longitude value is out of range.") 
        @>


[<Theory>]
[<InlineData("-10,-100,25,30")>]
[<InlineData("-50,20,25,100")>]
let ``Reports error when latitude value is out of bounds`` parameterValue =
    let result = parseImportArgs [ "--bounds"; parameterValue ]
    test <@ 
            result 
            |> isError ("'bounds' parameter's value is invalid, "
                        + "latitude value is out of range.") 
        @>


[<Fact>]
let ``Reports error when min and max longitude values are switched``() =
    let result = parseImportArgs [ "--bounds"; "80,10,70,30" ]
    test <@ 
            result 
            |> isError ("'bounds' parameter's value is invalid, "
                + "max longitude value is smaller than min longitude value.") 
        @>


[<Fact>]
let ``Reports error when min and max latitude values are switched``() =
    let result = parseImportArgs [ "--bounds"; "10,20,25,-10" ]
    test <@ 
            result 
            |> isError ("'bounds' parameter's value is invalid, "
                + "max latitude value is smaller than min latitude value.") 
        @>


[<Fact>]
let ``Parses valid bounds values``() =
    let result = parseImportArgs [ "--bounds"; "-10.1,20,25,70.4" ]
    test <@ 
            (result 
            |> parsedOptions).Bounds = 
                Some { MinLon = -10.1;
                    MinLat = 20.;
                    MaxLon = 25.;
                    MaxLat = 70.4 }
        @>


[<Fact>]
let ``The default SRTM dir is 'srtm``() =
    let result = parseImportArgs [ "--bounds"; "-10.1,20,25,70.4" ]
    test <@ 
            (result |> parsedOptions).SrtmDir = "srtm"
        @>


[<Fact>]
let ``Reports error when SRTM value is missing``() =
    let result = parseImportArgs [ "--srtm-dir" ]
    test <@ result |> isError "'srtm-dir' parameter's value is missing." @>


[<Fact>]
let ``Parses the SRTM dir parameter``() =
    let srtmDirValue = "somewhere/else"

    let result = 
        parseImportArgs [ 
        "--srtm-dir"; srtmDirValue
        "--bounds"; "-10.1,20,25,70.4";
        ]
    test <@ 
            (result |> parsedOptions).SrtmDir = srtmDirValue
        @>


[<Fact>]
let ``Imports all tiles within the specified boundaries``() =
    let tilesCoords = [
        { Lon = SrtmLongitude.fromInt 15; Lat = SrtmLatitude.fromInt 45 }
        { Lon = SrtmLongitude.fromInt 16; Lat = SrtmLatitude.fromInt 46 }
    ]

    let tileFiles = 
        tilesCoords 
        |> List.map (fun tc -> { TileCoords = tc; FileName = "somefile" } )

    let mutable tilesRead: SrtmTileHgtFile list = []
    let mutable heightsArraysProduced: HeightsArray list = []

    let readTile (tileFile: SrtmTileHgtFile): HeightsArray =
        tilesRead <- tileFile :: tilesRead
        let heightsArray = HeightsArray (0, 0, 10, 10, fun x -> None)
        heightsArraysProduced <- heightsArray :: heightsArraysProduced
        heightsArray

    let createPngFile tileId = new MemoryStream() :> Stream

    let mutable heightsArraysEncoded: HeightsArray list = []

    let pngEncoder heightsArray stream =
        heightsArraysEncoded <- heightsArray :: heightsArraysEncoded
        ignore()

    import tileFiles readTile createPngFile pngEncoder

    test <@ heightsArraysProduced.Length = tilesCoords.Length @>
    test <@ heightsArraysProduced = heightsArraysEncoded @>
