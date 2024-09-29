[<RequireQualifiedAccess>]
module Demeton.Dem.Xth

open System
open System.IO
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.IOTypes
open FileSys

/// <summary>
/// Reads HeightsArray data from a XTH-encoded stream.
/// </summary>
let readHeightsFromStream tileSize (stream: Stream) : DemHeight[] =
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
            heightFromLittleEndianBytes firstByte secondByte


    // Note that the SRTM tile has one pixel/width more of width
    // and height than we need (example: SRTM tiles of tile size 3600 are
    // 3601 of width & height).
    // When reading heights, we skip that additional row and column, so this
    // function returns tileSize*tileSize number of heights.

    let tileSizePlus1 = tileSize + 1

    let arraySize = tileSize * tileSize
    let heightsArray: DemHeight[] = Array.zeroCreate arraySize

    let mutable heightsReadCount = 0

    // The first line in the XTH format is the top (northern edge) line
    // that overlaps with the neighboring tile to the north.
    // This has two implications:
    //   1. We need to skip the first line of heights.
    //   2. We need to fill the heights array in the reverse order, since
    //      in the HeightsArray coordinate system the first line is the
    //      southern edge.

    // Skipping the first line of heights.
    stream.Seek(int64 tileSizePlus1 * 2L, SeekOrigin.Begin) |> ignore
    let streamReader = FunctionalStreamReader(stream)

    let mutable heightsArrayCellIndex = (tileSize - 1) * tileSize

    while heightsArrayCellIndex >= 0 && streamReader.moveForward () do
        let heightRead = readNextHeightFromStream streamReader

        heightsArray.[heightsArrayCellIndex] <- heightRead

        heightsArrayCellIndex <- heightsArrayCellIndex + 1

        // when we reach the end of the line, we move the index to one
        // line below the current one (due to the reverse order of filling
        // the heights array)
        if heightsArrayCellIndex % tileSize = 0 then
            heightsArrayCellIndex <- heightsArrayCellIndex - tileSize - tileSize
            // this also means we should skip the next XTH height since it
            // is an overlap to the neighboring tile to the east
            streamReader.moveForward () |> ignore
            readNextHeightFromStream streamReader |> ignore

    heightsArray


/// <summary>
/// Reads the <see cref="HeightsArray"/> of a SRTM tile from
/// a XTH file stream.
/// </summary>
let readHeightsArrayFromStream tileSize tileId stream =
    let srtmHeights = readHeightsFromStream tileSize stream

    let cellMinX, cellMinY = tileMinCell tileSize tileId

    HeightsArray(
        cellMinX,
        cellMinY,
        tileSize,
        tileSize,
        HeightsArrayDirectImport srtmHeights
    )


let writeHeightsArrayToStream (stream: Stream) (heightsArray: HeightsArray) =
    // the XTH file should be written in the reverse order of the lines
    // since the HeightsArray coordinate system is reverse from XTH
    let mutable arrayIndex = (heightsArray.Height - 1) * heightsArray.Width

    while arrayIndex >= 0 do
        stream
        |> Bnry.writeLittleEndianInt16 heightsArray.Cells[arrayIndex]
        |> ignore

        arrayIndex <- arrayIndex + 1

        if arrayIndex % heightsArray.Width = 0 then
            arrayIndex <- arrayIndex - heightsArray.Width - heightsArray.Width

    heightsArray


let writeHeightsArrayToFile
    (fileName: FileName)
    (heightsArray: HeightsArray)
    : FileName =
    Log.debug "Writing the DEM tile to %s..." fileName

    fileName |> Pth.directory |> ensureDirectoryExists |> ignore

    openFileToWrite fileName
    |> function
        | Ok stream ->
            use stream = stream

            heightsArray |> writeHeightsArrayToStream stream |> ignore

            fileName
        | Error error -> raise error.Exception
