module ``convertZippedHgtTileToPng tests``

open Demeton.HgtPng
open Demeton.Srtm

open Xunit
open Swensen.Unquote
open System.IO
open Demeton.DemTypes

let withZipFileEntry(): Stream =
    new MemoryStream() :> Stream

let expectToReadZipFileEntry 
    expectedZipFileName 
    expectedEntryName
    entryStreamToReturn
    zipFileName 
    entryName =
    test <@ zipFileName = expectedZipFileName @>
    test <@ entryName = expectedEntryName @>
    entryStreamToReturn

let readZipFileEntry entryStreamToReturn _ _ = entryStreamToReturn

let expectToCreateSrtmTileFromStream
    expectedTileCoords
    expectedStream
    heightsArrayToReturn
    tileSize 
    tileCoords 
    stream =

    test <@ tileSize = 3600 @>
    test <@ tileCoords = expectedTileCoords @>
    test <@ stream = expectedStream @>

    heightsArrayToReturn

[<Fact>]
let ``Opens HGT file entry in the zip file``() =
    let tileId = "N00E031"
    let tileCoords = parseTileId tileId
    let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
    let entryName = "N00E031.hgt"
    let pngFileName = "some/other/N00E031.png"

    let entryStream = withZipFileEntry()

    convertZippedHgtTileToPng
        (expectToReadZipFileEntry zipFileName entryName entryStream)
        (fun _ _ _ -> ignore())
        { TileCoords = tileCoords; FileName = zipFileName }
        pngFileName

[<Fact>]
let ``Reads the zipped HGT tile as heights array``() =
    let tileId = "N00E031"
    let tileCoords = parseTileId tileId
    let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
    let entryName = "N00E031.hgt"
    let pngFileName = "some/other/N00E031.png"

    let entryStream = withZipFileEntry()

    let heightsArray = HeightsArray(10, 20, 30, 40, (fun _ -> None))

    convertZippedHgtTileToPng
        (readZipFileEntry entryStream)
        (expectToCreateSrtmTileFromStream tileCoords entryStream heightsArray)
        { TileCoords = tileCoords; FileName = zipFileName }
        pngFileName

    //let zipArchive = ZipFile.OpenRead "sdds"
    //let entry = zipArchive.GetEntry "sds"
    //entry.