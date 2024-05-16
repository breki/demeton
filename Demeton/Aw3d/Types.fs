module Demeton.Aw3d.Types

[<Literal>]
let Aw3dDirName = "AW3D"

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

    member this.Aw3dTileName =
        let latSign =
            match this.TileY with
            | y when y >= 0 -> 'S'
            | _ -> 'N'

        let lonSign =
            match this.TileX with
            | x when x >= 0 -> 'E'
            | _ -> 'W'

        $"%c{latSign}%03d{abs this.TileX}%c{lonSign}%03d{abs this.TileY}"
