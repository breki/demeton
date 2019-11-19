module Demeton.Srtm.Downsampling

open Demeton.DemTypes
open Demeton.Srtm.Types
open Demeton.Srtm.Funcs
open Demeton.Srtm.Png

/// <summary>
/// Specifies which method will be used to downsample 2x2 lower level SRTM tiles
/// into one higher level tile. 
/// </summary>
type DownsamplingMethod =
    /// <summary>
    /// Takes the average of the 2x2 heights array.
    /// </summary>
    | Average
    /// <summary>
    /// Reserved for some future, better downsampling method.
    /// </summary>
    | SomeFutureMethod

/// <summary>
/// Lists the children SRTM tiles needed to downsample the specific tile. 
/// </summary>
let childrenTilesNeededForDownsampling
    (downsamplingMethod: DownsamplingMethod) (tile: SrtmTileId) =
    
    match downsamplingMethod with
    | Average -> invalidOp "todo"
    | SomeFutureMethod ->
        let childLevel = tile.Level.Value - 1
        let childTileX0 = tile.TileX * 2 - 1
        let childTileY0 = tile.TileY * 2 - 1

        [|
            for dy in 1 .. 4 do
                for dx in 1 .. 4 do
                    yield { 
                        Level = SrtmLevel.fromInt childLevel; 
                        TileX = childTileX0 + dx - 1;
                        TileY = childTileY0 + dy - 1 }
        |]

let downsampleHeightPoint (source: HeightsArray) x y = 
    let childX = x <<< 1
    let childY = y <<< 1

    source.heightAt (childX, childY)

/// <summary>
/// Creates a tile heights array by downsampling the provided lower-level
/// heights array that is at least double its size. 
/// </summary>
let downsampleTileHeightsArray 
    downsamplingMethod tileSize tile (heightsArray: HeightsArray)
    : HeightsArray =
    let (tileMinX, tileMinY) = tile |> tileMinCell tileSize

    match downsamplingMethod with
    | Average -> invalidOp "todo"
    | SomeFutureMethod -> 
        HeightsArray(tileMinX, tileMinY, tileSize, tileSize, 
            HeightsArrayInitializer2D (fun (x, y) -> 
                downsampleHeightPoint heightsArray x y))

/// <summary>
/// Constructs a lower-level heights array from children tiles. This array will
/// then be used to construct a higher-level heights array (by downsampling)
/// for the parent tile.
/// </summary>
let lowerLevelHeightsArrayNeededForDownsampling
    downsamplingMethod tileSize tile childrenHeightsArrays =

    match downsamplingMethod with
    | Average -> invalidOp "todo"
    | SomeFutureMethod -> 
        let (tileMinX, tileMinY) = tile |> tileMinCell tileSize
        let buffer = 1
        let childrenMinX = tileMinX * 2 - buffer
        let childrenMinY = tileMinY * 2 - buffer
        let childrenSize = tileSize * 2 + buffer * 2
         
        let mergedHeightsArrayRect: Raster.Rect =
            { MinX = childrenMinX; MinY = childrenMinY; 
            Width = childrenSize; Height = childrenSize } 
        childrenHeightsArrays |> Demeton.Dem.merge mergedHeightsArrayRect
        
/// <summary>
/// Constructs a heights array for a higher-level tile from the list of 
/// children lower-level tiles.
/// </summary>
type HigherLevelTileConstructor = 
    DownsamplingMethod -> SrtmTileId -> SrtmTileId list
     -> Result<HeightsArray option, string>

/// <summary>
/// Constructs a heights array for a higher-level tile from the list of 
/// children lower-level tiles by applying a downsampling algorithm.
/// </summary>
let constructHigherLevelTileHeightsArray 
    (tileSize: int)
    (localCacheDir: FileSys.DirectoryName)
    (readTilePngFile: SrtmPngTileReader): HigherLevelTileConstructor =
    fun downsamplingMethod tile (childrenTiles: SrtmTileId list) ->

    readPngTilesBatch localCacheDir readTilePngFile childrenTiles
    |> Result.map (fun heightsArrays ->
        Log.info "Merging all the read tiles into a single array..."
        lowerLevelHeightsArrayNeededForDownsampling
            downsamplingMethod
            tileSize
            tile
            heightsArrays)
    // after merging the tiles, make a parent tile by downsampling
    |> Result.map (fun heightsArray ->
        Log.info "Creating the higher-level tile by downsampling..."
        heightsArray
        |> Option.map
               (downsampleTileHeightsArray downsamplingMethod tileSize tile))
