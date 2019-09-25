module ``convertZippedHgtTileToPng tests``

open Demeton.Srtm.Png

open Xunit
open Swensen.Unquote
open System.IO
open Demeton.DemTypes
open Demeton.Srtm

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

let expectHeightsArrayToBeEncodedIntoPngFile
    expectedHeightsArray
    expectedPngFileName
    heightsArray 
    pngFileName =
    test <@ heightsArray = expectedHeightsArray @>
    test <@ pngFileName = expectedPngFileName @>

[<Fact>]
let ``Opens HGT file entry in the zip file``() =
    let tileId = "N00E031"
    let tileCoords = Tile.parseTileId tileId
    let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
    let entryName = "N00E031.hgt"
    let pngFileName = "some/other/N00E031.png"

    let entryStream = withZipFileEntry()

    convertZippedHgtTileToPng
        (expectToReadZipFileEntry zipFileName entryName entryStream)
        (fun _ _ _ -> ())
        (fun _ _ -> ())
        { TileCoords = tileCoords; FileName = zipFileName }
        pngFileName

[<Fact>]
let ``Reads the zipped HGT tile as heights array``() =
    let tileId = "N00E031"
    let tileCoords = Tile.parseTileId tileId
    let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
    let pngFileName = "some/other/N00E031.png"

    let entryStream = withZipFileEntry()

    let heightsArray = HeightsArray(10, 20, 30, 40, (fun _ -> None))

    convertZippedHgtTileToPng
        (readZipFileEntry entryStream)
        (expectToCreateSrtmTileFromStream tileCoords entryStream heightsArray)
        (fun _ _ -> ())
        { TileCoords = tileCoords; FileName = zipFileName }
        pngFileName

[<Fact>]
let ``Encodes the read SRTM heights array into PNG file``() =
    let tileId = "N00E031"
    let tileCoords = Tile.parseTileId tileId
    let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
    let pngFileName = "some/other/N00E031.png"

    let entryStream = withZipFileEntry()

    let heightsArray = HeightsArray(10, 20, 30, 40, (fun _ -> None))

    convertZippedHgtTileToPng
        (readZipFileEntry entryStream)
        (fun _ _ _ -> heightsArray)
        (expectHeightsArrayToBeEncodedIntoPngFile heightsArray pngFileName)
        { TileCoords = tileCoords; FileName = zipFileName }
        pngFileName

[<Fact>]
let ``Returns the read heights array``() =
    let tileId = "N00E031"
    let tileCoords = Tile.parseTileId tileId
    let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
    let pngFileName = "some/other/N00E031.png"

    let entryStream = withZipFileEntry()

    let heightsArray = HeightsArray(10, 20, 30, 40, (fun _ -> None))

    let returnedHeightsArray = 
        convertZippedHgtTileToPng
            (readZipFileEntry entryStream)
            (fun _ _ _ -> heightsArray)
            (fun _ _ -> ())
            { TileCoords = tileCoords; FileName = zipFileName }
            pngFileName

    test <@ returnedHeightsArray = heightsArray @>    