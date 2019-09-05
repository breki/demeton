module Demeton.Tests.``Reading SRTM HGT files``

open Demeton.DemTypes
open Demeton.SrtmTypes
open Demeton.Srtm

open System
open System.IO;

open FsUnit
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Can read SRTM heights``() =
    use stream = new MemoryStream([| 10uy; 0uy; 0uy; 0uy; 1uy; 1uy |])
    let heights = readSrtmHeightsFromStream stream |> Array.ofSeq

    heights |> should equal [| Some 2560s; Some 0s; Some 257s |]

[<Fact>]
let ``Can read null SRTM heights``() =
    use stream = new MemoryStream([| 0x80uy; 0uy; 10uy; 0uy |])
    let heights = readSrtmHeightsFromStream stream |> Array.ofSeq

    heights |> should equal [| None; Some 2560s |]

[<Fact>]
let ``Can handle negative SRTM heights``() =
    use stream = new MemoryStream([| 255uy; 0b10011100uy; 10uy; 0uy |])
    let heights = readSrtmHeightsFromStream stream |> Array.ofSeq

    heights |> should equal [| Some -100s; Some 2560s |]

[<Fact>]
let ``Can create heights array from SRTM heights sequence``() =
    let nextRandomHeight (rnd: Random): int16 =
        (int16) (rnd.Next 2500)

    let int16ToBigEndianBytes (value: int16): byte seq = 
        seq {
            yield (byte)value
            yield (byte)(value >>> 8)
        }

    let givenAByteArrayOfHeights() =
        let byteArray: byte[] = Array.zeroCreate (3601*3601*2)

        let rnd = System.Random(123)
        byteArray
        //[| 
        //    for i in 0 .. 3601*3601 
        //        do yield! (nextRandomHeight >> int16ToBigEndianBytes) rnd |]

    let createSrtmTileFromStream tileCoords stream =
        let heights1DArray = readSrtmHeightsFromStream stream |> Array.ofSeq

        let tileMinCoords = tileCellMinCoords tileCoords
        
        let inline heightFrom1DArray (cellCoords: GlobalCellCoords) =
            let arrayIndex = cellCoords.X - tileMinCoords.X
                            + (cellCoords.Y - tileMinCoords.Y) * 3601
            heights1DArray.[arrayIndex]

        HeightsArray(tileMinCoords, 3600, 3600, heightFrom1DArray)


    let heightFromByteArray byteOffset (byteArray: byte []) =
        let firstByte = byteArray.[byteOffset]
        let secondByte = byteArray.[byteOffset + 1]
        heightFromBytes firstByte secondByte


    let byteArray = givenAByteArrayOfHeights()

    let sampleHeight1 = byteArray |> heightFromByteArray 0
    let sampleHeight2 = byteArray |> heightFromByteArray 3601

    use stream = new MemoryStream(byteArray)

    let tile = 
        createSrtmTileFromStream { 
            Lon = SrtmLongitude.fromInt 16; Lat = SrtmLatitude.fromInt 45 } 
            stream

    test <@ tile.Width = 3600 @>
    test <@ tile.Height = 3600 @>
    test <@ tile.MinCoords.X = 702000 @>
    test <@ tile.MinCoords.Y = 486000 @>
    test <@ tile.Cells.[0,0] = sampleHeight1 @>
    test <@ tile.Cells.[0,1] = sampleHeight2 @>
