module Tests.Srtm.``encodeHeightsArrayIntoPngFile tests``

open Demeton.DemTypes
open Demeton.Srtm.Png
open Xunit
open Swensen.Unquote
open System.IO

[<Fact>]
let ``Creates the necessary directories for the local cache``() =
    let heightsArray = 
        HeightsArray(
            10, 20, 5, 5, HeightsArrayInitializer1D (fun _ -> DemHeightNone))
    let localCacheDir = @"dir1\dir2"
    let localCacheDirWithLevel = localCacheDir |> Pth.combine "0"
    let pngFileName = localCacheDirWithLevel |> Pth.combine "file.png"

    encodeHeightsArrayIntoPngFile 
        (fun dir -> 
            test <@ dir = localCacheDirWithLevel @>
            dir)
        (fun _ -> new MemoryStream() :> Stream)
        pngFileName
        heightsArray 

[<Fact>]
let ``Writes the encoded PNG image to the specified file``() =
    let heightsArray = 
        HeightsArray(
            10, 20, 5, 5, HeightsArrayInitializer1D (fun _ -> DemHeightNone))
    let localCacheDir = @"dir1\dir2"

    FileSys.deleteDirectoryIfExists localCacheDir |> ignore

    let pngFileName = 
        localCacheDir |> Pth.combine "0" |> Pth.combine "file.png"

    encodeHeightsArrayIntoPngFile 
        FileSys.ensureDirectoryExists
        FileSys.openFileToWrite
        pngFileName
        heightsArray 
    |> ignore

    use writtenFileStream = FileSys.openFileToRead pngFileName
    use memoryStream = new MemoryStream()
    writtenFileStream.CopyTo(memoryStream)

    test <@ memoryStream.Length = 68L @>
