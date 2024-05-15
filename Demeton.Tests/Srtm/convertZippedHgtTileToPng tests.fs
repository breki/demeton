module Tests.Srtm.``convertZippedHgtTileToPng tests``


open Swensen.Unquote
open System.IO
open Demeton.DemTypes

let withSomeZipFileEntry () = Ok(new MemoryStream() :> Stream)


let readZipFileEntry entryStreamToReturn _ _ = entryStreamToReturn

let readHeightsArrayFromStream _ _ _ =
    HeightsArray(
        10,
        11,
        5,
        6,
        HeightsArrayInitializer1D(fun _ -> DemHeightNone)
    )

let expectToCreateSrtmTileFromStream
    expectedTileCoords
    heightsArrayToReturn
    tileSize
    tileCoords
    _
    =

    test <@ tileSize = 3600 @>
    test <@ tileCoords = expectedTileCoords @>

    heightsArrayToReturn

let expectHeightsArrayToBeEncodedIntoPngFile
    expectedHeightsArray
    expectedPngFileName
    pngFileName
    (heightsArray: HeightsArray)
    =
    test <@ heightsArray = expectedHeightsArray @>
    test <@ pngFileName = expectedPngFileName @>
    heightsArray

// todo 0: uncomment this once we know what we're doing

// [<Fact>]
// let ``Opens HGT file entry in the zip file`` () =
//     let tileName = "N00E031"
//     let tileId = parseTileName tileName
//     let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
//     let entryName = "N00E031.hgt"
//
//     let entryStream = withSomeZipFileEntry ()
//
//     let expectToReadZipFileEntry
//         expectedZipFileName
//         expectedEntryName
//         entryStreamToReturn
//         zipFileName
//         entryName
//         =
//         test <@ zipFileName = expectedZipFileName @>
//         test <@ entryName = expectedEntryName @>
//
//         HeightsArray(
//             0,
//             0,
//             0,
//             0,
//             HeightsArrayInitializer1D(fun _ -> DemHeightNone)
//         )
//         |> Ok
//
//     readZippedHgtFile
//         FileSys.readZipFileEntry
//         tileId
//         zipFileName
//         (expectToReadZipFileEntry zipFileName entryName entryStream)
//
// [<Fact>]
// let ``Reads the zipped HGT tile as heights array`` () =
//     let tileName = "N00E031"
//     let tileId = parseTileName tileName
//     let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
//     let pngFileName = "some/other/N00E031.png"
//
//     let entryStream = withSomeZipFileEntry ()
//
//     let heightsArray =
//         HeightsArray(
//             10,
//             20,
//             30,
//             40,
//             HeightsArrayInitializer1D(fun _ -> DemHeightNone)
//         )
//
//     convertZippedHgtTileToPng
//         (readZipFileEntry entryStream)
//         (expectToCreateSrtmTileFromStream tileId heightsArray)
//         (fun _ heightsArray -> heightsArray |> Ok)
//         tileId
//         zipFileName
//         pngFileName
//
// [<Fact>]
// let ``Encodes the read SRTM heights array into PNG file`` () =
//     let tileName = "N00E031"
//     let tileId = parseTileName tileName
//     let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
//     let pngFileName = "some/other/N00E031.png"
//
//     let entryStream = withSomeZipFileEntry ()
//
//     let heightsArray =
//         HeightsArray(
//             10,
//             20,
//             30,
//             40,
//             HeightsArrayInitializer1D(fun _ -> DemHeightNone)
//         )
//
//     convertZippedHgtTileToPng
//         (readZipFileEntry entryStream)
//         (fun _ _ _ -> heightsArray)
//         (fun a b ->
//             expectHeightsArrayToBeEncodedIntoPngFile
//                 heightsArray
//                 pngFileName
//                 a
//                 b
//             |> Ok)
//         tileId
//         zipFileName
//         pngFileName
//
// [<Fact>]
// let ``Returns the read heights array`` () =
//     let tileName = "N00E031"
//     let tileId = parseTileName tileName
//     let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
//     let pngFileName = "some/other/N00E031.png"
//
//     let entryStream = withSomeZipFileEntry ()
//
//     let heightsArray =
//         HeightsArray(
//             10,
//             20,
//             30,
//             40,
//             HeightsArrayInitializer1D(fun _ -> DemHeightNone)
//         )
//
//     let returnedHeightsArray =
//         convertZippedHgtTileToPng
//             (readZipFileEntry entryStream)
//             (fun _ _ _ -> heightsArray)
//             (fun _ heightsArray -> Ok heightsArray)
//             tileId
//             zipFileName
//             pngFileName
//
//     test <@ returnedHeightsArray = Ok heightsArray @>
