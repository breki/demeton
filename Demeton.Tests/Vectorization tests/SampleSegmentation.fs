module Tests.``Vectorization tests``.SampleSegmentation

open Demeton.Vectorization.MarchingSquares

/// Segmentation function implemented as a elevation contours identifier.
let heightsSegmentation (heightsArray: int[,]) (isolineHeight: int)
    : SegmentationFunc = fun isolinePoint ->
    match isolinePoint with
    | HPoint (OnHorizontalEdge (x, y)) ->
        let hUp = heightsArray.[x, y]
        let hDown = heightsArray.[x, y + 1]
        match hUp, isolineHeight, hDown with
        | _ when hUp <= isolineHeight && isolineHeight < hDown ->
            HStep (OnHorizontalEdge (x, y), Right) |> Some
        | _ when hUp > isolineHeight && isolineHeight >= hDown ->
            HStep (OnHorizontalEdge (x, y), Left) |> Some
        | _ -> None
    | VPoint (OnVerticalEdge (x, y)) ->
        let hLeft = heightsArray.[x, y]
        let hRight = heightsArray.[x + 1, y]
        match hLeft, isolineHeight, hRight with
        | _ when hLeft <= isolineHeight && isolineHeight < hRight ->
            VStep (OnVerticalEdge (x, y), Up) |> Some
        | _ when hLeft > isolineHeight && isolineHeight >= hRight ->
            VStep (OnVerticalEdge (x, y), Down) |> Some
        | _ -> None

