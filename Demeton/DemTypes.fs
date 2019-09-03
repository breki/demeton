module Demeton.DemTypes 

type DemHeight = int16

type GlobalCellCoords = { X : int; Y : int }

type HeightCell = { Coords: GlobalCellCoords; Height : DemHeight option }

type HeightArray
    (
        minCoords: GlobalCellCoords,
        width: int, 
        height: int,
        initializer: (GlobalCellCoords -> DemHeight option)) =
    member this.MinCoords = minCoords
    member this.Width = width
    member this.Height = height
    member this.MaxX = minCoords.X + width - 1
    member this.MaxY = minCoords.Y + height - 1
    member this.Cells = 
        Array2D.init<DemHeight option> width height 
            (fun x y -> 
                initializer { X = minCoords.X + x; Y = minCoords.Y + y})
    
    member this.heightAt (coords: GlobalCellCoords) = 
        let height = this.Cells.[
            coords.X - this.MinCoords.X, coords.Y - this.MinCoords.Y]
        height
        

