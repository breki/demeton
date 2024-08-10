module Tests.Dem.``Reading and writing SRTM HGT files``

open Demeton.Commands
open Demeton.Dem
open Demeton.Dem.Types
open Demeton.Dem.Funcs

open System
open System.IO

open FsUnit
open Xunit
open Swensen.Unquote
open TestHelp

[<Fact>]
let ``Can read SRTM heights`` () =
    use stream =
        new MemoryStream(
            [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy;
               10uy
               0uy
               0uy
               0uy
               1uy
               1uy
               10uy
               1uy
               0uy
               1uy
               1uy
               1uy |]
        )

    let heights = Hgt.readHeightsFromStream 2 stream

    heights |> should equal [| 2561s; 1s; 2560s; 0s |]

[<Fact>]
let ``Can read null SRTM heights`` () =
    use stream =
        new MemoryStream([| 0uy; 0uy; 0uy; 0uy; 0x80uy; 0uy; 0uy; 0uy |])

    let heights = Hgt.readHeightsFromStream 1 stream

    heights |> should equal [| DemHeightNone |]

[<Fact>]
let ``Can handle negative SRTM heights`` () =
    use stream =
        new MemoryStream(
            [| 0uy; 0uy; 0uy; 0uy; 255uy; 0b10011100uy; 0uy; 0uy |]
        )

    let heights = Hgt.readHeightsFromStream 1 stream

    heights |> should equal [| -100s |]

[<Fact>]
let ``Can create heights array from SRTM heights sequence`` () =

    let givenAByteArrayOfHeights tileSize =
        let mutable nextHeightToUse = 0s

        let int16ToBigEndianBytes (value: int16) : byte * byte =
            (byte (value >>> 8), byte value)

        let arrayLength = (tileSize + 1) * (tileSize + 1)

        let byteArray: byte[] = Array.zeroCreate (arrayLength * 2)

        for i in 0 .. arrayLength - 1 do
            let height = nextHeightToUse
            nextHeightToUse <- nextHeightToUse + 1s

            let firstByte, secondByte = height |> int16ToBigEndianBytes

            byteArray.[i * 2] <- firstByte
            byteArray.[i * 2 + 1] <- secondByte

        byteArray


    let heightFromByteArray byteOffset (byteArray: byte[]) =
        let firstByte = byteArray.[byteOffset]
        let secondByte = byteArray.[byteOffset + 1]
        heightFromBytes firstByte secondByte


    let tileSize = 5
    let byteArray = givenAByteArrayOfHeights tileSize

    let topLeftHeight = byteArray |> heightFromByteArray ((tileSize + 1) * 2)

    let bottomRightHeight =
        byteArray
        |> heightFromByteArray (((tileSize + 1) * 2) * (tileSize + 1) - 2 * 2)

    use stream = new MemoryStream(byteArray)

    let heights =
        Hgt.readHeightsArrayFromStream tileSize (demTileId 0 16 45) stream

    test <@ heights.Width = tileSize @>
    test <@ heights.Height = tileSize @>
    test <@ heights.MinX = 16 * tileSize @>
    test <@ heights.MinY = 45 * tileSize @>
    test <@ heights.Cells.[tileSize * (tileSize - 1)] = topLeftHeight @>
    test <@ heights.Cells.[tileSize - 1] = bottomRightHeight @>

[<Fact>]
let ``Can read HGT file`` () =
    let tileSize = Demeton.Srtm.Funcs.SrtmTileSize

    let hgtFileNameOnly = "N46E015.hgt"
    let tileId = parseTileName hgtFileNameOnly.[0..6]

    use stream = sampleFileStream hgtFileNameOnly

    test <@ stream <> null @>

    let heights = Hgt.readHeightsArrayFromStream tileSize tileId stream
    test <@ heights.Width = tileSize @>
    test <@ heights.Height = tileSize @>

    // Sample cells to check the heights.
    // The expected height was measured using QGIS.
    let sampleCells =
        [ (15., 46.999716, 913s) // NW corner
          (15., 46., 466s) // SW corner
          (15.9997273, 46, 258s) // SE corner
          (15.9997273, 46.9997213, 282s) ] // NE corner

    sampleCells
    |> List.iter (fun (lon, lat, expectedHeight) ->
        let cellX = Math.Round(longitudeToCellX (float tileSize) lon, 0)
        let cellY = Math.Round(latitudeToCellY (float tileSize) lat, 0)
        let height = heights.heightAt (int cellX, int cellY)

        height |> should equal expectedHeight)


[<Fact>]
let ``Can write and then read HGT data`` () =
    let no = DemHeightNone

    let cells =
        [| 1s; 2s; 3s; no; 4s; 5s; 6s; no; 7s; 8s; 9s; no; no; no; no; no |]

    let heightsArray = HeightsArray(0, 0, 4, 4, HeightsArrayDirectImport cells)

    use stream = new MemoryStream()

    heightsArray |> Hgt.writeHeightsArrayToStream stream |> ignore

    stream.Seek(0L, SeekOrigin.Begin) |> ignore

    let readHeightsArray = Hgt.readHeightsFromStream 3 stream

    readHeightsArray |> should equal [| 1s; 2s; 3s; 4s; 5s; 6s; 7s; 8s; 9s |]
