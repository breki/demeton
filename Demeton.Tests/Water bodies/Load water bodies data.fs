module Tests.Water_bodies.Load_water_bodies_data


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
open Demeton.WorldCover.Fetch
open Demeton.WorldCover.Funcs

type WaterBodiesTile = HeightsArray

[<Literal>]
let WaterBodiesTileSize = WorldCoverTileSize

[<Literal>]
let WaterBodiesCacheSubdirName = "WaterBodies"


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


let decodeWaterBodiesTileFromPng tileSize tileId stream =
    let validateImageSize (ihdr: IhdrData) =
        match ihdr.Width, ihdr.Height with
        | tileWidth, tileHeight when
            tileWidth = tileSize && tileHeight = tileSize
            ->
            Ok ihdr
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
        let minX, minY = tileMinCell tileSize tileId

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
                tileSize,
                tileSize,
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


let unpackWaterBodiesPngTilesFromWorldCoverTiff
    fileExists
    downloadFile
    readWorldCoverTiffFile
    cacheDir
    worldCoverTileId
    =
    ensureWorldCoverFile cacheDir fileExists downloadFile worldCoverTileId
    |> ignore

    let worldCover3by3HeightsArray =
        readWorldCoverTiffFile cacheDir None worldCoverTileId

    // convert the WorldCover tile into water bodies tile
    let waterBodies3by3HeightsArray =
        worldCover3by3HeightsArray |> convertWorldCoverRasterToWaterMonochrome

    // todo 3: chop up the WorldCover tile into 9 1x1 water bodies tiles

    // let extract (extractBounds: Rect) (heightArray: HeightsArray) : HeightsArray =


    // todo 6: and save them as PNG files
    ()


/// <summary>
/// Loads the water bodies tile from the cache or downloads it from the web.
/// </summary>
/// <remarks>
/// For a given 1x1 tile ID the function first checks whether there is already
/// a PNG file (or a 'none' file). If there is, it should load it and return
/// it as heights array. If there is not, it should load the WorldCover tile,
/// convert it into water bodies heights array, chop it up into 1x1 tiles and
/// save them as PNG files. Then it should load the PNG file and return it as
/// heights array.
/// </remarks>
let loadWaterBodiesTile
    (fileExists: FileExistsChecker)
    (ensureDirectoryExists: DirectoryExistsEnsurer)
    (openFileToRead: FileReader)
    (openFileToWrite: FileWriter)
    decodeWaterBodiesTileFromPng
    unpackWaterBodiesPngTilesFromWorldCoverTiff
    cacheDir
    (availableWorldCoverTiles: Set<DemTileId>)
    (tileId: DemTileId)
    : WaterBodiesTile option =
    let cachedPngFileName =
        Path.Combine(
            cacheDir,
            WaterBodiesCacheSubdirName,
            $"WaterBodies-%s{toTileName tileId}.png"
        )

    let cachedNoneFileName = Path.ChangeExtension(cachedPngFileName, "none")

    // We implement the main body of work as a nested recursive function
    // since we need to call it again after we download the WorldCover tile
    // and chop it up into water bodies tiles.
    let rec loadCachedTileOrDownloadFromWeb () =
        if fileExists cachedPngFileName then
            match openFileToRead cachedPngFileName with
            | Ok stream ->
                decodeWaterBodiesTileFromPng WaterBodiesTileSize tileId stream
                |> Some
            | Error error -> Error error |> raise error.Exception
        else if fileExists cachedNoneFileName then
            None
        else
            let containingWorldCoverTileId =
                containingWorldCoverFileTileId tileId

            if
                (availableWorldCoverTiles
                 |> Set.contains containingWorldCoverTileId)
            then
                // todo 5: implement unpackWaterBodiesPngTilesFromWorldCoverTiff
                unpackWaterBodiesPngTilesFromWorldCoverTiff
                    cacheDir
                    containingWorldCoverTileId

                loadCachedTileOrDownloadFromWeb ()
            else
                match ensureDirectoryExists cacheDir with
                | Ok _ -> ()
                | Error error -> Error error |> raise error.Exception

                // Create a 'none' file and return None
                match openFileToWrite cachedNoneFileName with
                | Ok stream ->
                    stream |> closeStream
                    None
                | Error error -> Error error |> raise error.Exception

    loadCachedTileOrDownloadFromWeb ()


[<Literal>]
let CacheDir = "cache"

let someWorldCoverTiles = set []

/// <summary>
/// Tests that the water bodies heights array can be encoded into PNG via
/// a stream and then decoded back to the heights array.
/// </summary>
[<Fact>]
let ``Water bodies heights array can be encoded into PNG and back`` () =
    let tileSize = 100
    let tileId = demTileXYId 7 45
    let minX = longitudeToCellX tileSize 7
    let minY = latitudeToCellY tileSize 45

    let rnd = Random(Seed = 123)

    let waterBodiesData =
        HeightsArray(
            minX,
            minY,
            tileSize,
            tileSize,
            HeightsArrayInitializer1D(fun _ -> rnd.Next(0, 2) |> int16)
        )

    let stream =
        encodeWaterBodiesHeightsArrayToPng waterBodiesData (new MemoryStream())

    stream.Seek(0L, SeekOrigin.Begin) |> ignore

    let decodingResult = decodeWaterBodiesTileFromPng tileSize tileId stream

    match decodingResult with
    | Ok decodedWaterBodiesData ->
        test <@ decodedWaterBodiesData.MinX = waterBodiesData.MinX @>
        test <@ decodedWaterBodiesData.MinY = waterBodiesData.MinY @>
        test <@ decodedWaterBodiesData.Width = waterBodiesData.Width @>
        test <@ decodedWaterBodiesData.Height = waterBodiesData.Height @>

        test
            <@
                decodedWaterBodiesData.Cells.Length = waterBodiesData.Cells.Length
            @>

        test <@ decodedWaterBodiesData.Cells = waterBodiesData.Cells @>
    | Error error -> failwith error


// todo 30: implement this test
[<Fact>]
let ``Returns None if the tile is not covered by the underlying WorldCover tileset``
    ()
    =
    test <@ true @>


[<Fact>]
let ``If water bodies PNG tile is already in cache, return that one`` () =
    let fileExists fileName =
        test <@ fileName = @"cache\WaterBodies\WaterBodies-N45E007.png" @>
        true

    let openFileToRead fileName =
        test <@ fileName = @"cache\WaterBodies\WaterBodies-N45E007.png" @>
        Ok(new MemoryStream() :> Stream)

    let decodeWaterBodiesTileFromPng tileSize tileId stream =
        test <@ tileSize = WaterBodiesTileSize @>
        HeightsArray(0, 0, 1, 1, HeightsArrayInitializer1D(fun _ -> 0s))

    let waterBodiesTile =
        loadWaterBodiesTile
            fileExists
            Should.notBeCalled
            openFileToRead
            Should.notBeCalled
            decodeWaterBodiesTileFromPng
            Should.notBeCalled2
            CacheDir
            someWorldCoverTiles
            (demTileXYId 7 45)

    test <@ waterBodiesTile.IsSome @>

[<Fact>]
let ``If water bodies 'none' file is in cache, return None for the tile`` () =
    let fileExists fileName =
        match fileName with
        | @"cache\WaterBodies\WaterBodies-N45E007.none" -> true
        | _ -> false

    let waterBodiesTile =
        loadWaterBodiesTile
            fileExists
            Should.notBeCalled
            Should.notBeCalled
            Should.notBeCalled
            Should.notBeCalled2
            Should.notBeCalled2
            CacheDir
            someWorldCoverTiles
            (demTileXYId 7 45)

    test <@ waterBodiesTile.IsNone @>

[<Fact>]
let ``If 1x1 tile has no corresponding WorldCover tile, make 'none' file and return None``
    ()
    =
    let fileExists fileName = false

    let ensureDirExists dirName = Ok dirName

    let openFileToRead fileName =
        raise <| AssertionFailedException("Should not be called.")

    let openFileToWrite fileName =
        test <@ fileName = @"cache\WaterBodies\WaterBodies-N45E007.none" @>

        new MemoryStream() :> Stream |> Ok

    let unpackWaterBodiesPngTilesFromWorldCoverTiff (url: string) fileName =
        Should.fail "Unexpected file download"

    let waterBodiesTile =
        loadWaterBodiesTile
            fileExists
            ensureDirExists
            openFileToRead
            openFileToWrite
            Should.notBeCalled2
            unpackWaterBodiesPngTilesFromWorldCoverTiff
            CacheDir
            someWorldCoverTiles
            (demTileXYId 7 45)

    test <@ waterBodiesTile.IsNone @>


[<Fact>]
let ``Construct water bodies PNG tile from downloaded WorldCover TIFF tile``
    ()
    =
    let mutable tileCreated = false

    let fileExists fileName = tileCreated

    let ensureDirExists dirName = Ok dirName

    let openFileToRead fileName =
        test <@ fileName = @"cache\WaterBodies\WaterBodies-N45E007.png" @>
        Ok(new MemoryStream() :> Stream)

    let decodeWaterBodiesTileFromPng tileSize tileId stream =
        test <@ tileSize = WaterBodiesTileSize @>
        HeightsArray(0, 0, 1, 1, HeightsArrayInitializer1D(fun _ -> 0s))

    let unpackWaterBodiesPngTilesFromWorldCoverTiff (url: string) fileName =
        tileCreated <- true

    let tileId = demTileXYId 7 45
    let containingWorldCoverTileId = demTileXYId 6 45

    let waterBodiesTile =
        loadWaterBodiesTile
            fileExists
            Should.notBeCalled
            openFileToRead
            Should.notBeCalled
            decodeWaterBodiesTileFromPng
            unpackWaterBodiesPngTilesFromWorldCoverTiff
            CacheDir
            (Set.ofSeq [ containingWorldCoverTileId ])
            tileId

    test <@ waterBodiesTile.IsSome @>


[<Fact>]
let ``Downloads the WorldCover TIFF tile from the web`` () =
    let mutable fileDownloaded = false

    let fileExists _ = false

    let downloadFile _ _ =
        fileDownloaded <- true
        "some-file.tif"

    let readWorldCoverTiffFile cacheDir _ worldCoverTileId =
        HeightsArray(
            0,
            0,
            WorldCoverBitmapSize,
            WorldCoverBitmapSize,
            ZeroHeightsArray
        )

    unpackWaterBodiesPngTilesFromWorldCoverTiff
        fileExists
        downloadFile
        readWorldCoverTiffFile
        CacheDir
        (demTileXYId 6 45)

    test <@ fileDownloaded @>


[<Fact>]
let ``Unpacks TIFF heights array into 9 1x1 water bodies PNG tiles`` () =
    let fileExists _ = true

    let readWorldCoverTiffFile cacheDir _ worldCoverTileId =
        HeightsArray(
            0,
            0,
            WorldCoverBitmapSize,
            WorldCoverBitmapSize,
            ZeroHeightsArray
        )

    unpackWaterBodiesPngTilesFromWorldCoverTiff
        fileExists
        Should.notBeCalled2
        readWorldCoverTiffFile
        CacheDir
        (demTileXYId 6 45)

// todo 2: continue with the test
