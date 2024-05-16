module Tests.Srtm.``convertZippedHgtTileToPng tests``


open Swensen.Unquote
open Demeton.Dem.Types
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Xunit



let someHeightsArray =
    HeightsArray(
        10,
        11,
        3600,
        3600,
        HeightsArrayInitializer1D(fun _ -> DemHeightNone)
    )


[<Fact>]
let ``Opens the correct HGT file entry in the zip file`` () =
    let tileName = "N00E031"
    let tileId = parseTileName tileName
    let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
    let entryName = "N00E031.hgt"

    let expectToReadZipFileEntry actualZipFileName actualEntryName _ =
        test <@ actualZipFileName = zipFileName @>
        test <@ actualEntryName = entryName @>

        Ok someHeightsArray

    readZippedHgtFile expectToReadZipFileEntry tileId zipFileName (fun _ ->
        Ok someHeightsArray)

[<Fact>]
let ``Encodes the read SRTM heights array into PNG file`` () =
    let tileName = "N00E031"
    let tileId = parseTileName tileName
    let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
    let pngFileName = "some/other/N00E031.png"

    let expectHeightsArrayToBeEncodedIntoPngFile
        expectedPngFileName
        pngFileName
        (heightsArray: HeightsArray)
        =
        test <@ heightsArray = someHeightsArray @>
        test <@ pngFileName = expectedPngFileName @>
        heightsArray

    convertZippedHgtTileToPng
        (fun _ _ _ -> Ok someHeightsArray)
        (fun a b ->
            expectHeightsArrayToBeEncodedIntoPngFile pngFileName a b |> Ok)
        tileId
        zipFileName
        pngFileName

[<Fact>]
let ``Returns the read heights array`` () =
    let tileName = "N00E031"
    let tileId = parseTileName tileName
    let zipFileName = "some/dir/N00E031.SRTMGL1.hgt.zip"
    let pngFileName = "some/other/N00E031.png"

    let returnedHeightsArray =
        convertZippedHgtTileToPng
            (fun _ _ _ -> Ok someHeightsArray)
            (fun _ heightsArray -> Ok heightsArray)
            tileId
            zipFileName
            pngFileName

    test <@ returnedHeightsArray = Ok someHeightsArray @>
