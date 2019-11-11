module Tests.Srtm.``Fetching SRTM tiles``.``Checking_SRTM_dir_tile_status``

open Demeton.Srtm.Types
open Demeton.Srtm.Funcs

open Xunit
open Swensen.Unquote
open Tests.Srtm.SrtmHelper

let srtmDir = "somesrtm"

type SrtmDirTileStatus =
    | DoesNotExist
    | Exists

type SrtmDirTileStatusChecker = 
    SrtmLongitude -> SrtmLatitude -> SrtmDirTileStatus

let checkSrtmDirTileStatus srtmDir (fileExists: FileSys.FileExistsChecker)
    : SrtmDirTileStatusChecker = 
    fun lon lat ->
    { Level = SrtmLevel.fromInt 0; Lon = lon; Lat = lat }
    |> toZippedSrtmTileFileName srtmDir
    |> fileExists
    |> function
    | true -> Exists
    | false -> DoesNotExist

let whenFileExists fileNameThatExists: FileSys.FileExistsChecker 
    = fun fileNameToCheck -> fileNameToCheck = fileNameThatExists

[<Fact>]
let ``If tile is not in the SRTM storage, it is marked as not existing``() =
    test <@ 
            checkSrtmDirTileStatus 
                srtmDir
                (fun _ -> false)
                (SrtmLongitude.fromInt 10) 
                (SrtmLatitude.fromInt 20) = DoesNotExist @>


[<Fact>]
let ``If tile is in the SRTM storage, it is marked as existing``() =
    test <@ 
            checkSrtmDirTileStatus 
                srtmDir
                (whenFileExists 
                    (srtmDir |> Pth.combine "N20E010.SRTMGL1.hgt.zip"))
                (SrtmLongitude.fromInt 10) 
                (SrtmLatitude.fromInt 20) = Exists @>


