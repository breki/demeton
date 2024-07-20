module Tests.Water_bodies.Load_water_bodies_data

// todo 10: a new water bodies loaded function that for a given 1x1 tile ID
//   first checks whether there is already a PNG file (or a 'none' file).
//   If there is, it should load it and return it as heights array.
//   If there is not, it should load the WorldCover tile, convert it into
//   water bodies heights array, chop it up into 1x1 tiles and save them as PNG
//   files. Then it should load the PNG file and return it as heights array.



open System
open System.IO
open Demeton.Dem.Types
open FsUnit
open Png.Types
open Raster
open Xunit
open Swensen.Unquote
open Demeton.WorldCover.Types
open Demeton.Dem.Funcs
open FileSys


type WaterBodiesTile = HeightsArray

[<Literal>]
let WaterBodiesTileSize = WorldCoverTileSize



/// <summary>
/// Encodes the <see cref="HeightsArray" /> containing water bodies tile data into
/// a PNG image and writes it into the specified stream.
/// </summary>
/// <param name="heightsArray">
/// A <see cref="HeightsArray" /> that holds the water bodies data for a tile.
/// </param>
/// <param name="outputStream">
/// The output stream into which the PNG image will be written.
/// </param>
/// <returns>This same instance of the output stream.</returns>
let encodeWaterBodiesHeightsArrayToPng
    (heightsArray: HeightsArray)
    (outputStream: Stream)
    : Stream =

    let inline demHeight01ToBool demHeight =
        match demHeight with
        | 0s -> false
        | 1s -> true
        | _ -> raise <| ArgumentOutOfRangeException("demHeight")

    let imageData =
        heightsArrayToGrayscale1BitImageData demHeight01ToBool heightsArray

    let ihdr =
        { Width = heightsArray.Width
          Height = heightsArray.Height
          BitDepth = PngBitDepth.BitDepth1
          ColorType = PngColorType.Grayscale
          InterlaceMethod = PngInterlaceMethod.NoInterlace }

    outputStream |> Png.File.savePngToStream ihdr imageData


let decodeWaterBodiesTileFromPng tileId stream =
    let validateImageSize (ihdr: IhdrData) =
        match (ihdr.Width, ihdr.Height) with
        | WaterBodiesTileSize, WaterBodiesTileSize -> Ok ihdr
        | _, _ ->
            Error
                "The image size of this PNG does not correspond to the water bodies tile."

    let validateColorType (ihdr: IhdrData) =
        match ihdr.ColorType with
        | PngColorType.Grayscale -> Ok ihdr
        | _ ->
            Error
                "The color type of this PNG does not correspond to the water bodies tile."

    let validateBitDepth (ihdr: IhdrData) =
        match ihdr.BitDepth with
        | PngBitDepth.BitDepth1 -> Ok ihdr
        | _ ->
            Error
                "The bit depth of this PNG does not correspond to the water bodies tile."

    let generateHeightsArray (ihdr: IhdrData) (imageData: RawImageData) =
        let minX, minY = tileMinCell WaterBodiesTileSize tileId

        let waterBodiesTileInitialize (cells: DemHeight[]) =
            let totalPixelsCount = ihdr.Width * ihdr.Height

            let mutable pixelIndex = 0
            let mutable byteIndex = 0
            let mutable bitMask = 1uy <<< 7

            while pixelIndex < totalPixelsCount do
                let pixelValue = imageData.[byteIndex] &&& bitMask > 0uy

                cells.[pixelIndex] <- if pixelValue then 1s else 0s

                pixelIndex <- pixelIndex + 1
                bitMask <- bitMask >>> 1

                if pixelIndex % ihdr.Width = 0 then
                    byteIndex <- byteIndex + 1
                    bitMask <- 1uy <<< 7
                elif bitMask = 0uy then
                    bitMask <- 1uy <<< 7
                    byteIndex <- byteIndex + 1

        Ok(
            HeightsArray(
                minX,
                minY,
                WaterBodiesTileSize,
                WaterBodiesTileSize,
                HeightsArrayCustomInitializer waterBodiesTileInitialize
            )
        )

    let ihdr, imageData = stream |> Png.File.loadPngFromStream

    stream |> closeStream

    let validationResult =
        ResultSeq.fold
            [ validateColorType; validateBitDepth; validateImageSize ]
            ihdr

    match validationResult with
    | Ok _ -> generateHeightsArray ihdr imageData
    | Error errors -> Error(errors |> String.concat " ")


let loadWaterBodiesTile
    (fileExists: FileExistsChecker)
    cacheDir
    (tileId: DemTileId)
    : WaterBodiesTile option =
    // todo 10: implement this function
    let cachedFileName =
        Path.Combine(cacheDir, $"WaterBodies-%s{toTileName tileId}.png")

    if fileExists cachedFileName then
        // todo 5: implement reading of water bodies PNG tile
        HeightsArray(
            0,
            0,
            WaterBodiesTileSize,
            WaterBodiesTileSize,
            EmptyHeightsArray
        )
        |> Some
    else
        NotImplementedException() |> raise



[<Literal>]
let cacheDir = "cache"

[<Fact>]
let ``Water bodies heights array can be encoded into PNG and back`` () =
    let tileId = demTileXYId 7 45
    let minX = longitudeToCellX WaterBodiesTileSize 7
    let minY = latitudeToCellY WaterBodiesTileSize 45

    let rnd = Random(Seed=123)

    let waterBodiesData =
        HeightsArray(
            minX,
            minY,
            WaterBodiesTileSize,
            WaterBodiesTileSize,
            HeightsArrayInitializer1D (fun _ -> rnd.Next(0, 2) |> int16)
        )

    let stream =
        encodeWaterBodiesHeightsArrayToPng waterBodiesData (new MemoryStream())

    stream.Seek(0L, SeekOrigin.Begin) |> ignore

    let decodingResult = decodeWaterBodiesTileFromPng tileId stream

    match decodingResult with
    | Ok decodedWaterBodiesData ->
        test <@ decodedWaterBodiesData.MinX = waterBodiesData.MinX @>
        test <@ decodedWaterBodiesData.MinY = waterBodiesData.MinY @>
        test <@ decodedWaterBodiesData.Width = waterBodiesData.Width @>
        test <@ decodedWaterBodiesData.Height = waterBodiesData.Height @>
        test <@ decodedWaterBodiesData.Cells.Length = waterBodiesData.Cells.Length @>

        // sample random cells and compare them
        Array.zeroCreate 1000
        |> Array.map (fun _ -> rnd.Next(0, decodedWaterBodiesData.Cells.Length))
        |> Array.iter (fun i ->
            test <@ decodedWaterBodiesData.Cells[i] = waterBodiesData.Cells[i] @>)
    | Error error -> failwith error





// todo 30: implement this test
[<Fact>]
let ``Returns None if the tile is not covered by the underlying WorldCover tileset``
    ()
    =
    test <@ true @>


// todo 10: continue with the test
[<Fact>]
let ``If water bodies PNG tile is already in cache, return that one`` () =
    let fileExists fileName =
        test <@ fileName = @"cache\WaterBodies-N45E007.png" @>
        true

    let waterBodiesTile =
        loadWaterBodiesTile fileExists cacheDir (demTileXYId 7 45)

    test <@ waterBodiesTile.IsSome @>
