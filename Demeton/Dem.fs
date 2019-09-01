module Demeton.Dem

open DemTypes

let merge (heightArrays: HeightArray list): HeightArray option =
    match heightArrays with
    | [] -> None
    | [ array ] -> Some array
    | _ -> 
        let minX = heightArrays |> List.map (fun d -> d.MinX) |> List.min
        let minY = heightArrays |> List.map (fun d -> d.MinY) |> List.min
        let maxX = heightArrays |> List.map (fun d -> d.MaxX) |> List.max
        let maxY = heightArrays |> List.map (fun d -> d.MaxY) |> List.max
        let width = maxX - minX + 1
        let height = maxY - minY + 1

        Some (HeightArray(minX, minY, width, height, (fun x y -> None)))
