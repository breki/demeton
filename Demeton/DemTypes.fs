module Demeton.DemTypes 

type DemHeight = int

type HeightCell = { X : int; Y : int; Height : DemHeight option }

type HeightArray(
                    x: int, 
                    y: int, 
                    width: int, 
                    height: int,
                    cells: HeightCell list) =
    member this.MinX = x
    member this.MinY = y
    member this.Width = width
    member this.Height = height
    member this.MaxX = x + width - 1
    member this.MaxY = y + height - 1
    member this.Cells = Array2D.create<DemHeight option> width height None
    
    member this.heightAt x y = this.Cells.[x, y]

