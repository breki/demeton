module Demeton.DemTypes 

type Height = DemHeight of int

type NoHeight = NoHeight of unit

type HeightCell =
    | Height
    | NoHeight

type HeightArray(x: int, y: int, width: int, height: int) =
    member this.MinX = x
    member this.MinY = y
    member this.Width = width
    member this.Height = height
    member this.MaxX = x + width - 1
    member this.MaxY = y + height - 1
    member this.Cells = Array2D.create width height NoHeight



