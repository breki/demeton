module Demeton.Tests.``Commands tests``.``ImportSrtmTilesCommand tests``

open Demeton.Commands.ImportSrtmTilesCommand
open Demeton.SrtmTypes
open Demeton.DemTypes

open Xunit
open Swensen.Unquote
open System.IO


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
