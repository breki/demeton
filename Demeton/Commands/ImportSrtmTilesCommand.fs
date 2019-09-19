module Demeton.Commands.ImportSrtmTilesCommand
    
open Demeton.SrtmTypes
open Demeton.DemTypes
open System.IO

type SrtmToPngEncoder = HeightsArray -> Stream -> unit

let import 
    (tiles: SrtmTileCoords seq)
    (tileReader: SrtmTileReader)
    (localPngTileFileCreator: string -> Stream)
    (pngEncoder: SrtmToPngEncoder)
    : unit = 
    ignore()