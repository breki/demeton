module ``FileSystem tests``.Downloading_files

open FileSys
open Swensen.Unquote
open Xunit

[<Fact>]
let ``can download a file`` () =
    test
        <@
            downloadFile "http://httpbin.org/image/jpeg" "httpbin-image.jpg" = "httpbin-image.jpg"
        @>

[<Fact>]
let ``trying to download file without redirect support`` () =
    let result =
        downloadFileWithoutRedirects
            // this URL responds with 302 Found
            "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2404/N045W010/N047W010.zip"
            "redirected.html"

    test <@ result = None @>
