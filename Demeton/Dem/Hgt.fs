[<RequireQualifiedAccess>]
module Demeton.Dem.Hgt

open System
open System.IO
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.IOTypes

/// <summary>
/// Reads HeightsArray data from a HGT-encoded stream.
/// </summary>
let readFromStream tileSize (stream: Stream) : DemHeight[] =
    let inline readNextHeightFromStream (streamReader: FunctionalStreamReader) =
        let firstByte = streamReader.currentByte ()

        match streamReader.moveForward () with
        | false ->
            raise (
                InvalidOperationException(
                    "Unexpected end of SRTM heights stream reached."
                )
            )
        | true ->
            let secondByte = streamReader.currentByte ()
            heightFromBytes firstByte secondByte

    let streamReader = FunctionalStreamReader(stream)

    // Note that the SRTM tile has one pixel/width more of width
    // and height than we need (example: SRTM tiles of tile size 3600 are
    // 3601 of width & height).
    // When reading heights, we skip that additional row and column, so this
    // function returns tileSize*tileSize number of heights.

    let tileSizePlus1 = tileSize + 1

    // Total number of heights we need to read from the stream.
    // Note that this is _not_ the total number of heights in the SRTM tile,
    // since we skip the final column of heights.
    let heightsNeeded = tileSize * tileSizePlus1 - 1

    let arraySize = tileSize * tileSize
    let heightsArray: DemHeight[] = Array.zeroCreate arraySize

    let mutable heightsReadCount = 0
    let mutable heightsWrittenCount = 0

    while heightsReadCount < heightsNeeded && streamReader.moveForward () do
        let heightRead = readNextHeightFromStream streamReader

        // here we check whether the read height belongs to the final column
        // that we should skip
        match heightsReadCount % tileSizePlus1 = tileSize with
        | true -> () // not emitting the final column's height
        | false ->
            heightsArray.[heightsWrittenCount] <- heightRead
            heightsWrittenCount <- heightsWrittenCount + 1

        heightsReadCount <- heightsReadCount + 1

    heightsArray
