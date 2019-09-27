module Demeton.Tests.``Reading SRTM HGT files``

open Demeton.DemTypes
open Demeton.Srtm
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open System
open System.IO;
open System.Reflection

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Can read SRTM heights``() =
    use stream = new MemoryStream([| 
        10uy; 0uy; 0uy; 0uy; 1uy; 1uy 
        10uy; 1uy; 0uy; 1uy; 1uy; 1uy 
        10uy; 2uy; 0uy; 2uy; 1uy; 1uy 
        |])
    let heights = readSrtmHeightsFromStream 2 stream |> Array.ofSeq

    heights |> should equal [| 
        2560s; 0s
        2561s; 1s |]

[<Fact>]
let ``Can read null SRTM heights``() =
    use stream = new MemoryStream([| 
        0x80uy; 0uy; 10uy; 0uy 
        10uy; 0uy; 0uy; 0uy
        |])
    let heights = readSrtmHeightsFromStream 1 stream |> Array.ofSeq

    heights |> should equal [| DemHeightNone |]

[<Fact>]
let ``Can handle negative SRTM heights``() =
    use stream = new MemoryStream([| 
        255uy; 0b10011100uy; 10uy; 0uy 
        10uy; 0uy; 0uy; 0uy
        |])
    let heights = readSrtmHeightsFromStream 1 stream |> Array.ofSeq

    heights |> should equal [| -100s |]

[<Fact>]
let ``Can create heights array from SRTM heights sequence``() =
    let nextRandomHeight (rnd: Random): int16 =
        (int16) (rnd.Next 2500)

    let int16ToBigEndianBytes (value: int16): (byte*byte) = 
        ((byte)value, (byte)(value >>> 8))

    let givenAByteArrayOfHeights tileSize =
        let arrayLength = (tileSize + 1) * (tileSize + 1)

        let byteArray: byte[] = 
            Array.zeroCreate (arrayLength*2)

        let rnd = System.Random(123)

        for i in 0 .. arrayLength-1 do
            let (firstByte, secondByte) = 
                (nextRandomHeight >> int16ToBigEndianBytes) rnd

            byteArray.[i * 2] <- firstByte
            byteArray.[i * 2 + 1] <- secondByte

        byteArray


    let heightFromByteArray byteOffset (byteArray: byte []) =
        let firstByte = byteArray.[byteOffset]
        let secondByte = byteArray.[byteOffset + 1]
        heightFromBytes firstByte secondByte


    let tileSize = 5
    let byteArray = givenAByteArrayOfHeights tileSize

    let sampleHeight1 = byteArray |> heightFromByteArray 0
    let sampleHeight2 = byteArray |> heightFromByteArray ((tileSize + 1) * 2)

    use stream = new MemoryStream(byteArray)

    let tile = 
        createSrtmTileFromStream tileSize { 
            Lon = SrtmLongitude.fromInt 16; Lat = SrtmLatitude.fromInt 45 } 
            stream

    test <@ tile.Width = tileSize @>
    test <@ tile.Height = tileSize @>
    test <@ tile.MinX = (16 + 179) * tileSize @>
    test <@ tile.MinY = (45 + 90) * tileSize @>
    test <@ tile.Cells.[0] = sampleHeight1 @>
    test <@ tile.Cells.[tileSize] = sampleHeight2 @>

[<Fact>]
[<Trait("Category", "slow")>]
let ``Can read HGT file``() =
    let hgtFileNameOnly = "N00E031.hgt"
    let tileCoords = Tile.parseTileId hgtFileNameOnly.[0..6]

    let assembly = Assembly.GetExecutingAssembly()
    use stream = assembly.GetManifestResourceStream
                    ("Demeton.Tests.samples." + hgtFileNameOnly)

    test <@ stream <> null @>

    createSrtmTileFromStream 3600 tileCoords stream

