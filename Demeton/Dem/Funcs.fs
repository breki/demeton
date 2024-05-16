module Demeton.Dem.Funcs

open System
open Demeton.Dem.Types

let mbrOfHeightsArrays (heightsArrays: HeightsArray seq) : Raster.Rect =
    match heightsArrays |> Seq.isEmpty with
    | true -> Raster.Rect.Empty
    | false ->
        let minX = (heightsArrays |> Seq.minBy (fun d -> d.MinX)).MinX
        let minY = (heightsArrays |> Seq.minBy (fun d -> d.MinY)).MinY
        let maxX = (heightsArrays |> Seq.maxBy (fun d -> d.MaxX)).MaxX
        let maxY = (heightsArrays |> Seq.maxBy (fun d -> d.MaxY)).MaxY
        let width = maxX - minX + 1
        let height = maxY - minY + 1

        { MinX = minX
          MinY = minY
          Width = width
          Height = height }

let merge
    (mergedArrayBounds: Raster.Rect)
    (heightArrays: HeightsArray list)
    : HeightsArray option =

    let copyHeightsArray (source: HeightsArray) (dest: HeightsArray) : unit =
        let copyMinX = max source.MinX dest.MinX
        let copyMinY = max source.MinY dest.MinY
        let copyMaxX = min source.MaxX dest.MaxX
        let copyMaxY = min source.MaxY dest.MaxY

        for y in copyMinY..copyMaxY do
            for x in copyMinX..copyMaxX do
                source.heightAt (x, y) |> dest.setHeightAt (x, y)

    match (heightArrays, mergedArrayBounds.Width, mergedArrayBounds.Height) with
    | _, 0, _ -> None
    | _, _, 0 -> None
    | [], _, _ -> None
    | _ ->
        let merged =
            HeightsArray(
                mergedArrayBounds.MinX,
                mergedArrayBounds.MinY,
                mergedArrayBounds.Width,
                mergedArrayBounds.Height,
                EmptyHeightsArray
            )

        heightArrays |> List.iter (fun x -> copyHeightsArray x merged)

        Some merged

/// <summary>
/// Extracts a sub-array from the given array. If the sub-array is outside of
/// the bounds of the given array, the missing values are filled with
/// Int16.MinValue.
/// </summary>
let extract
    (extractBounds: Raster.Rect)
    (heightArray: HeightsArray)
    : HeightsArray =

    let getValue (x, y) : DemHeight =
        if
            x >= heightArray.MinX
            && x <= heightArray.MaxX
            && y >= heightArray.MinY
            && y <= heightArray.MaxY
        then
            // a marker that the water body pixel was not yet visited
            heightArray.heightAt (x, y)
        else
            // this is not a water body pixel
            Int16.MinValue

    HeightsArray(
        extractBounds.MinX,
        extractBounds.MinY,
        extractBounds.Width,
        extractBounds.Height,
        HeightsArrayInitializer2D getValue
    )
