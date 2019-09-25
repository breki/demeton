module Demeton.Tests.``Converting HGT to PNG``

open Demeton.DemTypes
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png
open Png.File

open FsUnit
open Xunit
open FsCheck.Xunit
open Swensen.Unquote

open System.IO
open System.Reflection


[<Fact>]
let ``DEM height of 0 is represented as unsigned int16 value of 32768``() =
    demHeightToUInt16Value (Some 0s) = 32768us


[<Fact>]
let ``DEM height of 1000 is represented as unsigned int16 value of 33768``() =
    demHeightToUInt16Value (Some 1000s) = 33768us


[<Fact>]
let ``Missing DEM height is represented as unsigned int16 value of 0``() =
    demHeightToUInt16Value (None) = 0us


[<Property>]
let ``DEM height is correctly converted to uint16``(height: DemHeight option)=
    let converted = demHeightToUInt16Value height
    let heightReconverted = uint16ValueToDemHeight converted
    heightReconverted = height


[<Fact>]
let ``Can convert HeightsArray to 16-bit grayscale``() =
    let rnd = System.Random(123)

    let heightsArray = 
        new HeightsArray(
            10, 15, 100, 150, (fun _ ->  Some ((int16)(rnd.Next(-100, 3000)))))

    let imageData = 
        heightsArray |> heightsArrayToImageData demHeightToUInt16Value
    test <@ Array.length imageData = 100 * 150 * 2 @>


[<Fact>]
[<Trait("Category", "slow")>]
let ``Can convert a HGT file into PNG image``() =
    let srtmTileId = "N46E015"
    let hgtFileNameOnly = srtmTileId + ".hgt"
    let tileCoords = parseTileId hgtFileNameOnly.[0..6]

    let assembly = Assembly.GetExecutingAssembly()
    use hgtStream = assembly.GetManifestResourceStream
                                ("Demeton.Tests.samples." + hgtFileNameOnly)

    let clock = new System.Diagnostics.Stopwatch()
    clock.Start()

    printfn ("Reading the heights array...")
    
    let heightsArray = createSrtmTileFromStream 3600 tileCoords hgtStream

    printfn "%d Encoding heights into the PNG..." clock.ElapsedMilliseconds

    let pngFileName = Path.GetFullPath(srtmTileId + ".png")
    use pngWriteStream = File.OpenWrite(pngFileName)
    
    printfn
        "%d Writing image to %s ..." clock.ElapsedMilliseconds pngFileName

    encodeSrtmHeightsArrayToPng heightsArray pngWriteStream |> ignore
    pngWriteStream.Close()

    printfn "%d Reading the image." clock.ElapsedMilliseconds

    let readSrtmImageData imageData = ignore()

    use pngReadStream = File.OpenRead(pngFileName)
    let (ihdr, imageData) =
        pngReadStream |> loadPngFromStream

    printfn "%d DONE." clock.ElapsedMilliseconds
