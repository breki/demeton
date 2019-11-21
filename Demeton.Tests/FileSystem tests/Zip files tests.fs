module ``FileSystem tests``.``Zip files tests``

open FileSys
open System.IO
open Swensen.Unquote
open TestHelp
open Xunit

let copySampleResourceFileToDisk resourceFileName diskFileName =
    use resourceStream = sampleFileStream resourceFileName
    use zipOutputFileStream = File.OpenWrite diskFileName
    resourceStream.CopyTo(zipOutputFileStream)
    zipOutputFileStream.Close()
    diskFileName

[<Fact>]
let ``Can read zip file entry``() =
    let zipFileName =
        copySampleResourceFileToDisk "N46E015.SRTMGL1.hgt.zip" "file1.zip"

    match FileSys.openZipFileEntry zipFileName "N46E015.hgt" with
    | Ok stream -> 
        use copiedStream = new MemoryStream()
        stream.CopyTo (copiedStream)

        test <@ copiedStream.Length = 25934402L @>
    | Error error -> error |> fileSysErrorMessage |> fail
    
[<Fact>]
let ``Returns an error if ZIP file does not exist``() =
    match FileSys.openZipFileEntry "doesnotexist.zip" "N46E015.hgt" with
    | Ok _ -> fail "this should never happen" 
    | Error error ->
        test <@ (error.Exception :? FileNotFoundException)  @>
    
[<Fact>]
let ``Returns an error if file entry in the ZIP file does not exist``() =
    let zipFileName =
        copySampleResourceFileToDisk "N46E015.SRTMGL1.hgt.zip" "file2.zip"

    match FileSys.openZipFileEntry zipFileName "doesnotexist" with
    | Ok _ -> fail "this should never happen" 
    | Error error ->
        test <@ (error.Exception :? FileNotFoundException)  @>
    
