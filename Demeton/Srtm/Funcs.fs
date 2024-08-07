module Demeton.Srtm.Funcs

open Demeton.Dem
open Demeton.Dem.Types
open Demeton.Dem.Funcs

open FileSys

[<Literal>]
let SrtmTileSize = 3600


/// <summary>
/// Reads the <see cref="HeightsArray"/> of a SRTM tile from
/// a HGT file stream.
/// </summary>
let createSrtmTileFromStream tileSize tileId stream =
    let srtmHeights = Hgt.readFromStream tileSize stream

    let cellMinX, cellMinY = tileMinCell tileSize tileId

    HeightsArray(
        cellMinX,
        cellMinY,
        tileSize,
        tileSize,
        HeightsArrayDirectImport srtmHeights
    )

let toZippedSrtmTileFileName (srtmDir: string) (tileCoords: DemTileCoords) =
    let tileId = demTileXYId tileCoords.Lon.Value tileCoords.Lat.Value

    srtmDir |> Pth.combine $"%s{tileId.FormatLat2Lon3}.SRTMGL1.hgt.zip"

let toLocalCacheTileFileName
    (localCacheDir: DirectoryName)
    (tileId: DemTileId)
    : FileName =
    let tilePngFileName = $"%s{toTileName tileId}.png"

    let levelDirName =
        tileId.Level.Value.ToString(
            System.Globalization.CultureInfo.InvariantCulture
        )

    localCacheDir |> Pth.combine levelDirName |> Pth.combine tilePngFileName
