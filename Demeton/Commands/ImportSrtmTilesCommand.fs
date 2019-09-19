module Demeton.Commands.ImportSrtmTilesCommand
    
open Demeton.SrtmTypes
open Demeton.DemTypes
open System.IO

type SrtmToPngEncoder = HeightsArray -> Stream -> unit

let import 
    (tiles: SrtmTileHgtFile seq)
    (readTile: SrtmTileReader)
    (createPngTileFile: string -> Stream)
    (encodeSrtmTileToPng: SrtmToPngEncoder)
    : unit = 

    for tileFile in tiles do
        let heightsArray = readTile tileFile
        let tileId = Demeton.Srtm.tileId tileFile.TileCoords

        use pngStream = createPngTileFile tileId
        encodeSrtmTileToPng heightsArray pngStream

    ignore()