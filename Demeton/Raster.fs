[<RequireQualifiedAccess>]
module Demeton.Raster

type Rect = { MinX: int; MinY: int; Width: int; Height: int }
    with
    static member asMinMax minX minY maxX maxY =
        let width = maxX - minX
        let height = maxY - minY
        { MinX = minX; MinY = minY; Width = width; Height = height }

    member this.MaxX = this.MinX + this.Width
    member this.MaxY = this.MinY + this.Height
