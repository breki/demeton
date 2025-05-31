module Tests.Dem.``Reading and writing SRTM XTH files``

open Demeton.Dem
open Demeton.Dem.Types
open Demeton.Dem.Funcs

open System.IO

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Can read SRTM heights`` () =
    use stream =
        new MemoryStream(
            [| 0uy
               0uy
               0uy
               0uy
               0uy
               0uy
               0uy
               10uy
               0uy
               0uy
               1uy
               1uy
               1uy
               10uy
               1uy
               0uy
               1uy
               1uy |]
        )

    let heights = Xth.readHeightsFromStream 2 stream

    heights |> should equal [| 2561s; 1s; 2560s; 0s |]

[<Fact>]
let ``Can read null SRTM heights`` () =
    use stream =
        new MemoryStream([| 0uy; 0uy; 0uy; 0uy; 0uy; 0x80uy; 0uy; 0uy |])

    let heights = Xth.readHeightsFromStream 1 stream

    heights |> should equal [| DemHeightNone |]

[<Fact>]
let ``Can handle negative SRTM heights`` () =
    use stream =
        new MemoryStream(
            [| 0uy; 0uy; 0uy; 0uy; 0b10011100uy; 255uy; 0uy; 0uy |]
        )

    let heights = Xth.readHeightsFromStream 1 stream

    heights |> should equal [| -100s |]

[<Fact>]
let ``Can create heights array from SRTM heights sequence`` () =

    let givenAByteArrayOfHeights tileSize =
        let mutable nextHeightToUse = 0s

        let int16ToLittleEndianBytes (value: int16) : byte * byte =
            (byte value, byte (value >>> 8))

        let arrayLength = (tileSize + 1) * (tileSize + 1)

        let byteArray: byte[] = Array.zeroCreate (arrayLength * 2)

        for i in 0 .. arrayLength - 1 do
            let height = nextHeightToUse
            nextHeightToUse <- nextHeightToUse + 1s

            let firstByte, secondByte = height |> int16ToLittleEndianBytes

            byteArray.[i * 2] <- firstByte
            byteArray.[i * 2 + 1] <- secondByte

        byteArray


    let heightFromByteArray byteOffset (byteArray: byte[]) =
        let firstByte = byteArray.[byteOffset]
        let secondByte = byteArray.[byteOffset + 1]
        heightFromLittleEndianBytes firstByte secondByte


    let tileSize = 5
    let byteArray = givenAByteArrayOfHeights tileSize

    let topLeftHeight = byteArray |> heightFromByteArray ((tileSize + 1) * 2)

    let bottomRightHeight =
        byteArray
        |> heightFromByteArray (((tileSize + 1) * 2) * (tileSize + 1) - 2 * 2)

    use stream = new MemoryStream(byteArray)

    let heights =
        Xth.readHeightsArrayFromStream tileSize (demTileId 0 16 45) stream

    test <@ heights.Width = tileSize @>
    test <@ heights.Height = tileSize @>
    test <@ heights.MinX = 16 * tileSize @>
    test <@ heights.MinY = 45 * tileSize @>
    test <@ heights.Cells.[tileSize * (tileSize - 1)] = topLeftHeight @>
    test <@ heights.Cells.[tileSize - 1] = bottomRightHeight @>

[<Fact>]
let ``Can write and then read XTH data`` () =
    let no = DemHeightNone

    let cells =
        [| 1s; 2s; 3s; no; 4s; 5s; 6s; no; 7s; 8s; 9s; no; no; no; no; no |]

    let heightsArray = HeightsArray(0, 0, 4, 4, HeightsArrayDirectImport cells)

    use stream = new MemoryStream()

    heightsArray |> Xth.writeHeightsArrayToStream stream |> ignore

    stream.Seek(0L, SeekOrigin.Begin) |> ignore

    let readHeightsArray = Xth.readHeightsFromStream 3 stream

    readHeightsArray |> should equal [| 1s; 2s; 3s; 4s; 5s; 6s; 7s; 8s; 9s |]


[<Fact>]
let ``Encoding water bodies info into DEM`` () =
    let testWidth = 3
    let testHeight = 3

    let demHeights: DemHeight[] =
        [| 0s; 0s; 100s; 100s; -100s; -100s; -1s; -2s; 1s |]

    let dem =
        HeightsArray(
            0,
            0,
            testWidth,
            testHeight,
            HeightsArrayDirectImport demHeights
        )

    let waterBodiesHeights: DemHeight[] =
        [| 0s; 1s; 0s; 1s; 0s; 1s; 0s; 1s; 0s |]

    let waterBodies =
        HeightsArray(
            0,
            0,
            testWidth,
            testHeight,
            HeightsArrayDirectImport waterBodiesHeights
        )

    let encoded = dem |> Xth.encodeWaterBodiesInfoIntoDem waterBodies

    test
        <@ encoded.Cells = [| 0s; 1s; 100s; 101s; -100s; -99s; -2s; -1s; 0s |] @>

[<Fact>]
let ``Encoding water bodies info into DEM - new encoding`` () =
    let testWidth = 3
    let testHeight = 3

    let demHeights: DemHeight[] =
        [| 0s; 0s; 100s; 100s; -100s; -100s; -1s; -2s; 1s |]

    let dem =
        HeightsArray(
            0,
            0,
            testWidth,
            testHeight,
            HeightsArrayDirectImport demHeights
        )

    let waterBodiesHeights: DemHeight[] =
        [| 0s; 1s; 0s; 1s; 0s; 1s; 0s; 1s; 0s |]

    let waterBodies =
        HeightsArray(
            0,
            0,
            testWidth,
            testHeight,
            HeightsArrayDirectImport waterBodiesHeights
        )

    let encoded = dem |> Xth.encodeWaterBodiesInfoIntoDem2 waterBodies

    test
        <@
            encoded.Cells = [| 500s
                               int16 (0x8000us ||| 500us)
                               600s
                               int16 (0x8000us ||| 600us)
                               400s
                               int16 (0x8000us ||| 400us)
                               499s
                               int16 (0x8000us ||| 498us)
                               501s |]
        @>
