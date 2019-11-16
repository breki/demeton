module Tests.Srtm.``Fetching SRTM tiles``.``Checking SRTM dir tile status``

open Demeton.Srtm.Types
open Demeton.Srtm.Fetch

open Xunit
open Swensen.Unquote

let srtmDir = "somesrtm"

let whenFileExists fileNameThatExists: FileSys.FileExistsChecker 
    = fun fileNameToCheck -> fileNameToCheck = fileNameThatExists

[<Fact>]
let ``If tile is not in the SRTM storage, it is marked as not existing``() =
    test <@ 
            checkSrtmDirTileStatus 
                srtmDir
                (fun _ -> false)
                { Lon = SrtmLongitude.fromInt 10;
                   Lat = SrtmLatitude.fromInt 20 } = DoesNotExist @>


[<Fact>]
let ``If tile is in the SRTM storage, it is marked as existing``() =
    test <@ 
            checkSrtmDirTileStatus 
                srtmDir
                (whenFileExists 
                    (srtmDir |> Pth.combine "N20E010.SRTMGL1.hgt.zip"))
                { Lon = SrtmLongitude.fromInt 10;
                Lat = SrtmLatitude.fromInt 20 } = Exists @>


