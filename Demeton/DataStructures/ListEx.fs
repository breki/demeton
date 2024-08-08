module DataStructures.ListEx

let insertAt index item list =
    let left, right = list |> List.splitAt index
    (left @ [ item ]) @ right

/// Returns a copy of the list without the item at the specified index.
let removeAt itemIndex list =
    let left, right = list |> List.splitAt itemIndex
    List.append left (right |> List.tail)
