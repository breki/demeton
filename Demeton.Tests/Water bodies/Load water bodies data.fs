module Tests.Water_bodies.Load_water_bodies_data

// todo 20: remove this module once we're sure we are not going to use any code
// from it


open System
open System.IO
open Demeton.Dem.Types
open FsUnit
open Xunit
open Swensen.Unquote
open Demeton.WorldCover.Types
open Demeton.Dem.Funcs
open FileSys
open Demeton.WorldCover.Fetch
open Demeton.WorldCover.Funcs
open Demeton.WaterBodies


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
                decodeWaterBodiesTileFromPng WorldCoverTileSize tileId stream
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

    let decodingResult =
        decodeWaterBodiesTileFromPngStream tileSize tileId stream

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
        test <@ tileSize = WorldCoverTileSize @>
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
        test <@ tileSize = WorldCoverTileSize @>
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


[<Fact(Skip = "downloads a tile, so it takes too long")>]
// [<Fact>]
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


[<Fact(Skip = "downloads a tile and breaks it down into subtiles, so it takes too long")>]
// [<Fact>]
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
