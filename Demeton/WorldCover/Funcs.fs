module Demeton.WorldCover.Funcs

open Demeton.Dem.Types
open Demeton.Dem.Funcs


/// <summary>
/// Converts the original WorldCover raster to a monochrome raster where water
/// is represented by 1 and everything else by 0.
/// </summary>
/// <param name="heightsArray"></param>
let convertWorldCoverRasterToWaterMonochrome
    (heightsArray: HeightsArray)
    : HeightsArray =
    let waterToMonochrome value =
        match value with
        // 80 represents water
        | 80s -> 1s
        | _ -> 0s

    heightsArray |> mapRasterValues waterToMonochrome
