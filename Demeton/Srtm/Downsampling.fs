/// <summary>
/// Contains types and functions for creating a higher-level SRTM tiles from
/// lower-level ones by downsampling.
/// </summary>
module Demeton.Srtm.Downsampling

// Some references used in investigation, could be helpful in the future:
// - links:
//    - https://en.wikipedia.org/wiki/Image_scaling
//    - https://en.wikipedia.org/wiki/Supersampling
//    - https://www.geospatialworld.net/article/comparison-of-decimation-and-averaging-methods-of-dems-resampling/
// - terms: resampling, downsampling, gridding, interpolation methods,
//    nearest neighbor, bilinear, cubic convultion, bicubic interpolation,
//    decimation, 3x3 grid neighbourhood, kernel
// - code: https://www.paulinternet.nl/?page=bicubic

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
    
    let (childTileX0, childTileY0, childCols, childRows) =
        match downsamplingMethod with
        | Average ->
            let childTileX0 = tile.TileX * 2
            let childTileY0 = tile.TileY * 2
            (childTileX0, childTileY0, 2, 2)

        | SomeFutureMethod ->
            let childTileX0 = tile.TileX * 2 - 1
            let childTileY0 = tile.TileY * 2 - 1
            (childTileX0, childTileY0, 4, 4)

    let childLevel = tile.Level.Value - 1 |> SrtmLevel.fromInt
    
    [|
        for dy in 1 .. childCols do
            for dx in 1 .. childRows do
                yield { 
                    Level = childLevel; 
                    TileX = childTileX0 + dx - 1;
                    TileY = childTileY0 + dy - 1 }
    |]

/// <summary>
/// Downsamples the 2x2 heights grid by calculating its average (ignoring any
/// missing heights). 
/// </summary>
let downsampleAverage
    (height1: DemHeight) (height2: DemHeight)
    (height3: DemHeight) (height4: DemHeight): DemHeight =
    let average (sum: int) count =
        ((sum |> float) / (count |> float))
        |> System.Math.Round |> int16
    
    if height1 <> DemHeightNone
        && height2 <> DemHeightNone
        && height3 <> DemHeightNone
        && height4 <> DemHeightNone then
        average
            ((height1 |> int) + (height2 |> int)
             + (height3 |> int) + (height4 |> int))
            4
    else        
        let (sum, nonMissingHeightsCount) =
            [| height1; height2; height3; height4 |] 
            |> Array.fold (fun (sumSoFar, usedHeights) height -> 
                match height with
                | DemHeightNone -> (sumSoFar, usedHeights)
                | _ -> (sumSoFar + (height |> int), usedHeights + 1)
                )
                (0, 0)

        match nonMissingHeightsCount with
        | 0 -> DemHeightNone
        | _ -> average sum nonMissingHeightsCount


/// <summary>
/// Creates a tile heights array by downsampling the provided lower-level
/// heights array that is at least double its size. 
/// </summary>
let downsampleTileHeightsArray 
    downsamplingMethod tileSize tile (heightsArray: HeightsArray)
    : HeightsArray =
    let (tileMinX, tileMinY) = tile |> tileMinCell tileSize

    match downsamplingMethod with
    | Average ->
        HeightsArray(tileMinX, tileMinY, tileSize, tileSize, 
            HeightsArrayInitializer2D (fun (x, y) ->
                downsampleAverage
                    (heightsArray.heightAt(x * 2, y * 2))
                    (heightsArray.heightAt(x * 2 + 1, y * 2))
                    (heightsArray.heightAt(x * 2, y * 2 + 1))
                    (heightsArray.heightAt(x * 2 + 1, y * 2 + 1))
                )
            )
        
    | SomeFutureMethod ->
        invalidOp "to be implemented in the future, perhaps"

/// <summary>
/// Constructs a lower-level heights array from children tiles. This array will
/// then be used to construct a higher-level heights array (by downsampling)
/// for the parent tile.
/// </summary>
let lowerLevelHeightsArrayNeededForDownsampling
    downsamplingMethod tileSize tile childrenHeightsArrays =

    let (tileMinX, tileMinY) = tile |> tileMinCell tileSize

    // The size of the buffer (in number of cells) around the direct children
    // of the tile. This buffer is needed for certain downsampling methods
    // which operate on a NxN neighborhood grid.
    let buffer =
        match downsamplingMethod with
        | Average -> 0
        | SomeFutureMethod -> 1
        
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
