module Demeton.Dem

open DemTypes

let merge (heightArrays: HeightsArray list): HeightsArray option =
    let isCellWithinArray (array: HeightsArray) (cellCoords: GlobalCellCoords) =
        cellCoords.X >= array.MinCoords.X && cellCoords.X <= array.MaxX 
            && cellCoords.Y >= array.MinCoords.Y && cellCoords.Y <= array.MaxY

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
        : DemHeight option =
        match arrayMaybe with
        | Some array -> 
            let height = array.heightAt cellCoords
            height
        | None -> None

    match heightArrays with
    | [] -> None
    | [ array ] -> Some array
    | _ -> 
        let minX = heightArrays |> List.map (fun d -> d.MinCoords.X) |> List.min
        let minY = heightArrays |> List.map (fun d -> d.MinCoords.Y) |> List.min
        let maxX = heightArrays |> List.map (fun d -> d.MaxX) |> List.max
        let maxY = heightArrays |> List.map (fun d -> d.MaxY) |> List.max
        let width = maxX - minX + 1
        let height = maxY - minY + 1

        let heightOfCellInArrays coords = 
            heightArrays |> findArrayOfCell coords |> findHeightOfCell coords

        Some (
            HeightsArray(
                { X = minX; Y = minY }, width, height, heightOfCellInArrays))
