module Demeton.Hillshading

open Demeton
open Demeton.GeometryTypes
open System.IO

let hillshade (bounds: Bounds): Stream option =
    let neededTiles = Srtm.boundsToTiles bounds

    let neededTilesFiles = Srtm.ensureTilesAreInCache neededTiles

    None
