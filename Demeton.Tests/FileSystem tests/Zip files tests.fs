module ``FileSystem tests``.``Zip files tests``

open FileSys
open System.IO
open Swensen.Unquote
open TestHelp
open Xunit

[<Fact>]
let ``Can read zip file entry``() = 
    let zipFileName = "file.zip"

    use resourceStream = sampleFileStream "N46E015.SRTMGL1.hgt.zip"
    use zipOutputFileStream = File.OpenWrite(zipFileName)
    resourceStream.CopyTo(zipOutputFileStream)
    zipOutputFileStream.Close()

    match FileSys.openZipFileEntry zipFileName "N46E015.hgt" with
    | Ok stream -> 
        use copiedStream = new MemoryStream()
        stream.CopyTo (copiedStream)

        test <@ copiedStream.Length = 25934402L @>
    | Error error -> error |> fileSysErrorMessage |> fail
    