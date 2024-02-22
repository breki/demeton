module Tests.Aw3d.``AW3D experiments``


open FsUnit
open Xunit
open TestHelp
open BitMiracle.LibTiff.Classic
open Swensen.Unquote

[<Fact>]
let ``Icebreaker`` () =
    use tiffReadStream = sampleFileStream @"AW3D.ALPSMLC30_N046E006_DSM.tif"

    test <@ tiffReadStream <> null @>

    // load GeoTIFF image from stream
    use tiffFile =
        Tiff.ClientOpen("InMemory", "r", tiffReadStream, TiffStream())

    let width = unbox (tiffFile.GetField(TiffTag.IMAGEWIDTH).[0].Value)
    let height = unbox (tiffFile.GetField(TiffTag.IMAGELENGTH).[0].Value)

    test <@ width = 3600 @>
    test <@ height = 3600 @>

    let samplesPerPixel: int16 =
        unbox (tiffFile.GetField(TiffTag.SAMPLESPERPIXEL).[0].Value)

    let bitsPerSample: int16 =
        unbox (tiffFile.GetField(TiffTag.BITSPERSAMPLE).[0].Value)

    let scanlineSize = tiffFile.ScanlineSize()
    let buffer = Array.zeroCreate<byte> scanlineSize

    for row in 0 .. height - 1 do
        let success = tiffFile.ReadScanline(buffer, row)
        test <@ success @>

        // Process the buffer to get the pixel data
        for col in 0 .. width - 1 do
            let pixelStart = col * int samplesPerPixel * (int bitsPerSample / 8)
            let pixelData = Array.sub buffer pixelStart (int samplesPerPixel)

            // pixelData now contains the data for the pixel at (col, row)
            // You can process this data as needed

            let samplePixel = pixelData.[0]

            if row = 0 && col = 0 then
                test <@ samplePixel = 179uy @>
