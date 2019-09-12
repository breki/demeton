module Demeton.Tests.``Converting HGT to PNG``

open Demeton.DemTypes
open Demeton.Srtm
open Demeton.PngTypes
open Demeton.PngPixelFormats

open Xunit
open Swensen.Unquote

open System.IO
open System.Reflection


/// <summary>
/// Converts the <see cref="HeightsArray" /> into 16-bit grayscale image data.
/// </summary>
/// <param name="heightsArray">
/// A <see cref="HeightsArray" /> that holds heights data to be converted.
/// </param>
/// <returns>Image data.</returns>
let heightsArrayToImageData (heightsArray: HeightsArray)
    : Grayscale16BitImageData =

    let inline demHeightToUInt16Value (demHeight: DemHeight option): uint16 =
        let missingHeightAsUint16 = 0us
        let zeroHeight = 2s <<< 15

        match demHeight with
        | Some height -> (uint16)((int16)height + zeroHeight)
        | _ -> missingHeightAsUint16

    Array2D.init 
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

    let imageData: Grayscale16BitImageData = 
        heightsArrayToImageData heightsArray

    outputStream 
    |> saveGrayscale16BitToStream imageData
    |> ignore

    outputStream


[<Fact>]
let ``Can convert HeightsArray to 16-bit grayscale``() =
    let rnd = System.Random(123)

    let heightsArray = 
        new HeightsArray({ X = 10; Y = 15}, 
            100, 150, (fun _ ->  Some ((int16)(rnd.Next(-100, 3000)))))

    let imageData = heightsArray |> heightsArrayToImageData
    test <@ Array2D.length1 imageData = 100 @>
    test <@ Array2D.length2 imageData = 150 @>


// todo too slow
[<Fact>]
[<Trait("Category", "integration")>]
let ``Can convert a HGT file into PNG image``() =
    let srtmTileId = "N00E031"
    let hgtFileNameOnly = srtmTileId + ".hgt"
    let tileCoords = parseTileId hgtFileNameOnly.[0..6]

    let assembly = Assembly.GetExecutingAssembly()
    use hgtStream = assembly.GetManifestResourceStream
                                ("Demeton.Tests.samples." + hgtFileNameOnly)
    
    let heightsArray = createSrtmTileFromStream 3600 tileCoords hgtStream

    let pngFileName = srtmTileId + ".png";
    use pngWriteStream = File.OpenWrite(pngFileName)
    encodeSrtmHeightsArrayToPng  heightsArray pngWriteStream |> ignore
    pngWriteStream.Close()

    let readSrtmImageData imageData = ignore()

    use pngReadStream = File.OpenRead(pngFileName)
    pngReadStream
    |> loadPngFromStream (fun _ -> ()) readSrtmImageData
    |> ignore

