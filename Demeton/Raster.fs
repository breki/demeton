[<RequireQualifiedAccess>]
module Demeton.Raster

type Rect = { MinX: int; MinY: int; Width: int; Height: int }
    with
    static member asMinMax minX minY maxX maxY =
        let width = maxX - minX + 1
        let height = maxY - minY + 1
        { MinX = minX; MinY = minY; Width = width; Height = height }
