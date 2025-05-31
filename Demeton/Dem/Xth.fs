[<RequireQualifiedAccess>]
module Demeton.Dem.Xth

open System
open System.IO
open Demeton.Dem.Types
open Demeton.Dem.Funcs
open Demeton.IOTypes
open FileSys

/// <summary>
/// The minimum height value supported by the XTH format.
/// </summary>
let MIN_HEIGHT = -500s

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

            stream |> flushStream |> closeStream

            fileName
        | Error error -> raise error.Exception



/// <summary>
/// Encodes the water bodies information into the DEM heights array.
/// </summary>
/// <remarks>
/// The method first subtracts the height value from the absolute minimum
/// supported height value (which is -500) to ensure that the height
/// values stored are non-negative. For elevations on Earth this means
/// the value will be stored in the lower 14 bits.
/// The method then uses the MSB of the height value to encode the water bodies
/// information.
/// If the bit is set to 1, the cell represents water. If it is set to 0,
/// the cell represents a land.
/// </remarks>
let encodeWaterBodiesInfoIntoDem
    (waterBodiesHeightsArray: HeightsArray)
    (demHeightsArray: HeightsArray)
    : HeightsArray =
    if
        waterBodiesHeightsArray.Width <> demHeightsArray.Width
        || waterBodiesHeightsArray.Height <> demHeightsArray.Height
    then
        invalidArg
            "waterBodiesHeightsArray"
            (sprintf
                "The water bodies array (%d,%d) must have the same dimensions as the DEM array (%d,%d)."
                waterBodiesHeightsArray.Width
                waterBodiesHeightsArray.Height
                demHeightsArray.Width
                demHeightsArray.Height)

    for index in 0 .. waterBodiesHeightsArray.Cells.Length - 1 do
        let height = demHeightsArray.Cells.[index]

        if height < MIN_HEIGHT then
            invalidArg
                "demHeightsArray"
                (sprintf
                    "The height value %d at index %d is below the minimum supported height of %d."
                    height
                    index
                    MIN_HEIGHT)
        else
            let heightRelative = uint16 (height - MIN_HEIGHT)

            let waterBody = waterBodiesHeightsArray.Cells.[index]

            let heightValueEncoded =
                match waterBody with
                | 1s -> // water body
                    heightRelative ||| 0x8000us // set MSB to 1
                | 0s -> // land, nothing to do
                    heightRelative
                | _ ->
                    invalidArg
                        "waterBodiesHeightsArray"
                        (sprintf
                            "The water body value %d at index %d is not valid. It must be either 0 (land) or 1 (water)."
                            waterBody
                            index)

            demHeightsArray.Cells.[index] <- int16 heightValueEncoded

    demHeightsArray
