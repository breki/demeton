module Demeton.WorldCover.Types


[<Literal>]
let WorldCoverYear = 2021

[<Literal>]
let WorldCoverVersion = "v200" // year 2021

[<Literal>]
let WorldCoverS3Domain = "https://esa-worldcover.s3.eu-central-1.amazonaws.com"

let geoJsonUrl = $"{WorldCoverS3Domain}/esa_worldcover_grid.geojson"

[<Literal>]
let WorldCoverDirName = "WorldCover"

[<Literal>]
let WorldCoverTileSize = 12000

[<Literal>]
let WorldCoverBitmapSize = WorldCoverTileSize * 3


type WorldCoverTileName = string

type WorldCoverTileId =
    {
        /// <summary>
        /// Represents the X coordinate of the WorldCover tile.
        /// This is an integer value which is positive for tiles east of the
        /// Greenwich meridian and negative for tiles west of it.
        /// </summary>
        TileX: int

        /// <summary>
        /// Represents the Y coordinate of the WorldCover tile.
        /// This is an integer value which is positive for tile south of the
        /// Equator and negative for tiles north of it.
        /// </summary>
        TileY: int
    }

    member this.TileName: WorldCoverTileName =
        let latSign =
            match this.TileY with
            | x when x >= 0 -> 'N'
            | _ -> 'S'

        let lonSign =
            match this.TileX with
            | x when x >= 0 -> 'E'
            | _ -> 'W'

        $"%c{latSign}%02d{abs this.TileX}%c{lonSign}%03d{abs this.TileY}"
