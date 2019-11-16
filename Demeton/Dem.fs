module Demeton.Dem

open DemTypes

let merge (heightArrays: HeightsArray list): HeightsArray option =
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

    match heightArrays with
    | [] -> None
    | [ array ] -> Some array
    | _ -> 
        let minX = (heightArrays |> List.minBy (fun d -> d.MinX)).MinX
        let minY = (heightArrays |> List.minBy (fun d -> d.MinY)).MinY
        let maxX = (heightArrays |> List.maxBy (fun d -> d.MaxX)).MaxX
        let maxY = (heightArrays |> List.maxBy (fun d -> d.MaxY)).MaxY
        let width = maxX - minX + 1
        let height = maxY - minY + 1

        let heightOfCellInArrays = 
            HeightsArrayInitializer2D(fun coords -> 
                heightArrays 
                |> findArrayOfCell coords 
                |> findHeightOfCell coords)

        HeightsArray(minX, minY, width, height, heightOfCellInArrays)
        |> Some
