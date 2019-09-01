module Demeton.Dem

open DemTypes

let merge (heightArrays: HeightArray list): HeightArray option =
    match heightArrays with
    | [] -> None
    | [ array ] -> Some array
    | [ array1; array2 ] -> 
        let minX = min array1.MinX array2.MinX
        let minY = min array1.MinY array2.MinY
        let maxX = max array1.MaxX array2.MaxX
        let maxY = max array1.MaxY array2.MaxY
        let width = maxX - minX + 1
        let height = maxY - minY + 1
        Some (HeightArray(minX, minY, width, height, []))
    | _ -> 
        let minX = heightArrays |> List.map (fun d -> d.MinX) |> List.min
        let minY = heightArrays |> List.map (fun d -> d.MinY) |> List.min
        let maxX = heightArrays |> List.map (fun d -> d.MaxX) |> List.max
        let maxY = heightArrays |> List.map (fun d -> d.MaxY) |> List.max
        let width = maxX - minX + 1
        let height = maxY - minY + 1
        Some (HeightArray(minX, minY, width, height, []))
