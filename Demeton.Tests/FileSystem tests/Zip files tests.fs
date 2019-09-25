module ``Zip_files_tests``

open System.IO
open System.Reflection
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Can read zip file entry``() = 
    let zipFileName = "file.zip"

    let assembly = Assembly.GetExecutingAssembly()
    use resourceStream = 
        assembly.GetManifestResourceStream(
            "Demeton.Tests.samples.N46E015.SRTMGL1.hgt.zip")
    use zipOutputFileStream = File.OpenWrite(zipFileName)
    resourceStream.CopyTo(zipOutputFileStream)
    zipOutputFileStream.Close()

    use stream = FileSys.openZipFileEntry zipFileName "N46E015.hgt"
    use copiedStream = new MemoryStream()
    stream.CopyTo (copiedStream)

    test <@ copiedStream.Length = 25934402L @>
    