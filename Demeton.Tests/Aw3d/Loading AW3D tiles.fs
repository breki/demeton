module Tests.Aw3d.Loading_AW3D_tiles

open Demeton.Geometry.Common
open Demeton.Srtm.Funcs

open FsUnit
open Xunit
open Swensen.Unquote

type Aw3dTileId =
    {
        /// <summary>
        /// Represents the X coordinate of the AW3D tile.
        /// This is an integer value which is positive for tiles east of the
        /// Greenwich meridian and negative for tiles west of it.
        /// </summary>
        TileX: int

        /// <summary>
        /// Represents the Y coordinate of the AW3D tile.
        /// This is an integer value which is positive for tile south of the
        /// Equator and negative for tiles north of it.
        /// </summary>
        TileY: int
    }

    member this.Aw3dTileName() =
        let latSign =
            match this.TileY with
            | x when x >= 0 -> 'N'
            | _ -> 'S'

        let lonSign =
            match this.TileX with
            | x when x >= 0 -> 'E'
            | _ -> 'W'

        $"%c{latSign}%03d{abs this.TileX}%c{lonSign}%03d{abs this.TileY}"


/// <summary>
/// Given a bounding box, returns a sequence of AW3D tiles that cover it.
/// </summary>
let boundsToAw3dTiles (bounds: LonLatBounds) : Aw3dTileId seq =
    let cellsPerDegree = 3600
    let tileSize = 3600

    let minTileX =
        bounds.MinLon
        |> longitudeToCellX cellsPerDegree
        |> cellXToTileX tileSize
        |> floor
        |> int

    let minTileY =
        bounds.MaxLat
        |> latitudeToCellY cellsPerDegree
        |> cellYToTileY tileSize
        |> floor
        |> int

    let maxTileX =
        (bounds.MaxLon
         |> longitudeToCellX cellsPerDegree
         |> cellXToTileX tileSize
         |> ceil
         |> int)
        - 1

    let maxTileY =
        (bounds.MinLat
         |> latitudeToCellY cellsPerDegree
         |> cellYToTileY tileSize
         |> ceil
         |> int)
        - 1

    seq {
        for tileY in [ minTileY..maxTileY ] do
            for tileX in [ minTileX..maxTileX ] do
                yield { TileX = tileX; TileY = tileY }
    }

/// <summary>
/// Returns the download URL for the given AW3D tile.
/// </summary>
let aw3dTileDownloadUrl (tile_d: Aw3dTileId) : string =
    let groupTileX = tile_d.TileX / 5 * 5
    let groupTileY = tile_d.TileY / 5 * 5

    let groupTileId =
        { TileX = groupTileX
          TileY = groupTileY }

    let latSign = if groupTileY >= 0 then "N" else "S"
    let lonSign = if groupTileX >= 0 then "E" else "W"

    sprintf
        "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2303/%s/%s.zip"
        (groupTileId.Aw3dTileName())
        (tile_d.Aw3dTileName())



[<Fact>]
let ``Correctly calculates the AW3D tiles needed for a given boundary`` () =
    let bounds =
        { MinLon = 46.1
          MinLat = -8.1
          MaxLon = 47.9
          MaxLat = -6.9 }

    test
        <@
            boundsToAw3dTiles bounds |> Seq.toList = [ { TileX = 46; TileY = 6 }
                                                       { TileX = 47; TileY = 6 }
                                                       { TileX = 46; TileY = 7 }
                                                       { TileX = 47; TileY = 7 }
                                                       { TileX = 46; TileY = 8 }
                                                       { TileX = 47; TileY = 8 } ]
        @>


[<Theory>]
[<InlineData(46,
             6,
             "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2303/N045E005/N046E006.zip")>]
[<InlineData(-36,
             -120,
             "https://www.eorc.jaxa.jp/ALOS/aw3d30/data/release_v2303/S035W120/S036W120.zip")>]
let ``For a given AW3D tile, construct its download URL``
    tileX
    tileY
    expectedUrl
    =
    test
        <@ aw3dTileDownloadUrl { TileX = tileX; TileY = tileY } = expectedUrl @>


// todo 6: for a given list of AW3D tiles and the cache dir, download all
//   missing ones
