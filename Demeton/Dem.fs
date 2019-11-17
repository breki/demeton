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
    let isCellWithinArray (array: HeightsArray) ((cx, cy): GlobalCellCoords) =
        cx >= array.MinX && cx <= array.MaxX 
            && cy >= array.MinY && cy <= array.MaxY

    let findArrayOfCell 
        (cellCoords: GlobalCellCoords)
        (arrays: HeightsArray list) =
        arrays 
        |> List.tryFind (fun array -> isCellWithinArray array cellCoords)

    /// <summary>
    /// Looks for the height of the specific cell in the 
    /// <see cref="HeightArray"/>. Returns the height value or <c>None</c>
    /// if there is no height information for that cell.
    /// </summary>
    let findHeightOfCell 
        (cellCoords: GlobalCellCoords) (arrayMaybe: HeightsArray option)
        : DemHeight =
        match arrayMaybe with
        | Some array -> array.heightAt cellCoords
        | None -> DemHeightNone

    match (heightArrays, mergedArrayBounds.Width, mergedArrayBounds.Height) with
    | (_, 0, _) -> None
    | (_, _, 0) -> None
    | ([], _, _) -> None
    | _ -> 
        let heightOfCellInArrays = 
            HeightsArrayInitializer2D(fun coords -> 
                heightArrays 
                |> findArrayOfCell coords 
                |> findHeightOfCell coords)

        HeightsArray
            (mergedArrayBounds.MinX,
             mergedArrayBounds.MinY,
             mergedArrayBounds.Width,
             mergedArrayBounds.Height,
             heightOfCellInArrays)
        |> Some
