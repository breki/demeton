module Tests.Srtm.``encodeHeightsArrayIntoPngFile tests``

open Demeton.DemTypes
open Demeton.Srtm.Png
open FileSys
open Xunit
open Swensen.Unquote
open System.IO
open TestHelp

[<Fact>]
let ``Creates the necessary directories for the local cache``() =
    let heightsArray = 
        HeightsArray(
            10, 20, 5, 5, HeightsArrayInitializer1D (fun _ -> DemHeightNone))
    let localCacheDir = Pth.combine "dir1" "dir2"
    let localCacheDirWithLevel = localCacheDir |> Pth.combine "0"
    let pngFileName = localCacheDirWithLevel |> Pth.combine "file.png"

    writeHeightsArrayIntoPngFile 
        (fun dir -> 
            test <@ dir = localCacheDirWithLevel @>
            Ok dir)
        (fun _ -> new MemoryStream() :> Stream |> Ok)
        pngFileName
        heightsArray 

[<Fact>]
let ``Writes the encoded PNG image to the specified file``() =
    let heightsArray = 
        HeightsArray(
            10, 20, 5, 5, HeightsArrayInitializer1D (fun _ -> DemHeightNone))
    let localCacheDir = Pth.combine "dir1" "dir2"

    FileSys.deleteDirectoryIfExists localCacheDir |> ignore

    let pngFileName = 
        localCacheDir |> Pth.combine "0" |> Pth.combine "file1.png"

    writeHeightsArrayIntoPngFile 
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite
        pngFileName
        heightsArray 
    |> ignore

    match FileSys.openFileToRead pngFileName with
    | Ok writtenFileStream -> 
        use memoryStream = new MemoryStream()
        writtenFileStream.CopyTo(memoryStream)

        test <@ memoryStream.Length = 68L @>
    | Error error -> error |> fileSysErrorMessage |> fail
