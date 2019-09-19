module Demeton.Commands.ImportSrtmTilesCommand
    
open Demeton.SrtmTypes
open Demeton.DemTypes
open System.IO

type SrtmToPngEncoder = HeightsArray -> Stream -> unit

/// <summary>
/// A generic method for importing the specified SRTM tiles into the local
/// storage, encoded as PNG files.
/// </summary>
/// <param name="tiles">A sequence of SRTM tiles to import.</param>
/// <param name="readTile">
/// A function that transforms a SRTM tile into <see cref="HeightsArray" />.
/// This typically means the function should download the zipped HGT file,
/// unzip it and read it as <see cref="HeightsArray" /> instance.
/// </param>
/// <param name="createPngTileFile">
/// A function that creates a new PNG file for the given SRTM tile ID and
/// provides a writable stream to it.
/// </param>
/// <param name="encodeSrtmTileToPng">
/// A function that encodes the <see cref="HeightsArray" /> as a PNG image and
/// writes it into the provides stream.
/// </param>
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