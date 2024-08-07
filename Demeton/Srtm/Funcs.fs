module Demeton.Srtm.Funcs

open Demeton.Dem.Types
open Demeton.Dem.Funcs

open FileSys

[<Literal>]
let SrtmTileSize = 3600

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
