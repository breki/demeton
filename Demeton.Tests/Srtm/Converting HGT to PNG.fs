module Demeton.Tests.``Converting HGT to PNG``

open Demeton.DemTypes
open Demeton.Srtm
open Demeton.PngTypes
open Demeton.PngPixelFormats

open FsUnit
open Xunit
open FsCheck.Xunit
open Swensen.Unquote

open System.IO
open System.Reflection


let missingHeightAsUint16 = 0us
let zeroHeight = 1s <<< 15

let inline demHeightToUInt16Value (demHeight: DemHeight option): uint16 =
    match demHeight with
    | Some height -> (uint16)((int16)height + zeroHeight)
    | _ -> missingHeightAsUint16


let inline uint16ValueToDemHeight (value: uint16): DemHeight option =
    if value = missingHeightAsUint16 then None
    else Some (DemHeight ((int16)value - zeroHeight))


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

/// <summary>
/// Converts the <see cref="HeightsArray" /> into 16-bit grayscale image data.
/// </summary>
/// <param name="heightsArray">
/// A <see cref="HeightsArray" /> that holds heights data to be converted.
/// </param>
/// <returns>Image data.</returns>
let heightsArrayToImageData (heightsArray: HeightsArray): ImageData =

    grayscale16BitImageData
        heightsArray.Width
        heightsArray.Height
        (fun x y -> heightsArray.Cells.[x, y] |> demHeightToUInt16Value)


/// <summary>
/// Encodes the <see cref="HeightsArray" /> containing SRTM tile data into
/// a PNG image and writes it into the specified stream.
/// </summary>
/// <param name="heightsArray">
/// A <see cref="HeightsArray" /> that holds heights data of the SRTM tile.
/// </param>
/// <param name="outputStream">
/// The output stream into which the PNG image will be written.
/// </param>
/// <returns>This same instance of the output stream.</returns>
let encodeSrtmHeightsArrayToPng 
    (heightsArray: HeightsArray)
    (outputStream: Stream): Stream =

    let imageData = heightsArrayToImageData heightsArray

    let ihdr = { 
        Width = heightsArray.Width
        Height = heightsArray.Height
        BitDepth = PngBitDepth.BitDepth16
        ColorType = PngColorType.Grayscale
        InterlaceMethod = PngInterlaceMethod.NoInterlace
        }

    outputStream 
    |> savePngToStream ihdr imageData


[<Fact>]
let ``Can convert HeightsArray to 16-bit grayscale``() =
    let rnd = System.Random(123)

    let heightsArray = 
        new HeightsArray(
            10, 15, 100, 150, (fun _ ->  Some ((int16)(rnd.Next(-100, 3000)))))

    let imageData = heightsArray |> heightsArrayToImageData
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

    //let rnd = new System.Random(123)
    //let heightsArray = 
    //    HeightsArray(0, 0, 1000, 500,
    //        fun _ -> Some ((int16)(rnd.Next(10000))))

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
