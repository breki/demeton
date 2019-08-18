module Demeton.DemTypes 

type DemHeight = DemHeight of int

type NoHeight = NoHeight of unit

type DemCell =
    | DemHeight
    | NoHeight

type DemData(width, height) =
    member this.Cells = Array2D.create width height NoHeight



