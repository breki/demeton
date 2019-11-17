module Demeton.Dem

open DemTypes

let mbrOfHeightsArrays (heightsArrays: HeightsArray seq): Raster.Rect =
    match heightsArrays |> Seq.isEmpty with
    | true -> Raster.Rect.Empty
    | false ->
        let minX = (heightsArrays |> Seq.minBy (fun d -> d.MinX)).MinX
        let minY = (heightsArrays |> Seq.minBy (fun d -> d.MinY)).MinY
        let maxX = (heightsArrays |> Seq.maxBy (fun d -> d.MaxX)).MaxX
        let maxY = (heightsArrays |> Seq.maxBy (fun d -> d.MaxY)).MaxY
        let width = maxX - minX + 1
        let height = maxY - minY + 1
        { MinX = minX; MinY = minY; Width = width; Height = height; }

let merge
    (mergedArrayBounds: Raster.Rect)
    (heightArrays: HeightsArray list)
    : HeightsArray option =

    let copyHeightsArray (source: HeightsArray) (dest: HeightsArray): unit =
        let copyMinX = max source.MinX dest.MinX
        let copyMinY = max source.MinY dest.MinY
        let copyMaxX = min source.MaxX dest.MaxX
        let copyMaxY = min source.MaxY dest.MaxY
        
        for y in copyMinY .. copyMaxY do
            for x in copyMinX .. copyMaxX do
                source.heightAt (x, y) |> dest.setHeightAt (x, y) 
    
    match (heightArrays, mergedArrayBounds.Width, mergedArrayBounds.Height) with
    | (_, 0, _) -> None
    | (_, _, 0) -> None
    | ([], _, _) -> None
    | _ -> 
        let merged =
            HeightsArray
                (mergedArrayBounds.MinX,
                 mergedArrayBounds.MinY,
                 mergedArrayBounds.Width,
                 mergedArrayBounds.Height,
                 EmptyHeightsArray)
        
        heightArrays |> List.iter (fun x -> copyHeightsArray x merged) 
        
        Some merged
