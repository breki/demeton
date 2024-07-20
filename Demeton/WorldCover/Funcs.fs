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


// todo 30: we need to implement a better downsampling algorithm, this one is not
//   working well for large downsampling factors.
//   Possible solutions:
//       1. lower the threshold (although the maxWeightedSum is already quite low
//          right now, just 10%)
//       2. downsample in multiple passes - 2 by 2 until we reach the final step.
//          For example, if the factor is 10, we would do it 2 * 2 * 2.5
/// <summary>
/// Downsample the water bodies heights array by the given factor.
/// </summary>
let downsampleWaterBodiesHeightsArray
    (factor: float)
    (heightsArray: HeightsArray)
    : HeightsArray =
    let downsampledMinX = int (float heightsArray.MinX * factor)
    let downsampledMinY = int (float heightsArray.MinY * factor)
    let downsampledWidth = int (float heightsArray.Width * factor)
    let downsampledHeight = int (float heightsArray.Height * factor)

    let downsampledHeightsArray =
        HeightsArray(
            downsampledMinX,
            downsampledMinY,
            downsampledWidth,
            downsampledHeight,
            EmptyHeightsArray
        )

    let maxWeightedSum = ref 0.0

    for y in 0 .. downsampledHeight - 1 do
        for x in 0 .. downsampledWidth - 1 do
            // Calculate the exact original coordinates this new pixel maps to
            let origStartX = (float x / factor)
            let origEndX = (float (x + 1) / factor)
            let origStartY = (float y / factor)
            let origEndY = (float (y + 1) / factor)

            let mutable weightedSum = 0.0
            let mutable totalWeight = 0.0

            // Iterate through the original pixels that overlap with the new pixel
            for oy in int origStartY .. int origEndY do
                for ox in int origStartX .. int origEndX do
                    // Calculate the overlap area (weight) for each original pixel
                    let left = max origStartX (float ox)
                    let right = min origEndX (float ox + 1.0)
                    let top = max origStartY (float oy)
                    let bottom = min origEndY (float oy + 1.0)

                    let weight = (right - left) * (bottom - top)

                    if
                        weight > 0.0 && heightsArray.heightAtLocal (ox, oy) = 1s
                    then
                        weightedSum <- weightedSum + weight

                    totalWeight <- totalWeight + weight

            maxWeightedSum := max !maxWeightedSum weightedSum

            // Set the new pixel value based on the weighted average
            let weightRatio =
                if totalWeight > 0.0 then weightedSum / totalWeight else 0.0

            let downsampledValue = if weightRatio > 0.5 then 1s else 0s

            downsampledHeightsArray.setHeightAtLocal (x, y) downsampledValue

    downsampledHeightsArray
