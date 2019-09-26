module Demeton.DemTypes 

type DemHeight = int16
let inline DemHeight x = int16 x

type GlobalCellCoords = (int * int)

type HeightCell = { Coords: GlobalCellCoords; Height : DemHeight option }

type HeightsArray
    (
        minX: int,
        minY: int,
        width: int, 
        height: int,
        initializer: (GlobalCellCoords -> DemHeight option)) =
    let cells = 
        Array2D.init<DemHeight option> width height 
            (fun x y -> initializer (minX +  x, minY + y))
    member this.MinX = minX
    member this.MinY = minY
    member this.Width = width
    member this.Height = height
    member this.MaxX = minX + width - 1
    member this.MaxY = minY + height - 1
    member this.Cells = cells
    
    member this.heightAt ((x, y): GlobalCellCoords) = 
        let height = this.Cells.[
            x - this.MinX, y - this.MinY]
        height
        
/// <summary>
/// A result of an operation that can return an optional 
/// <see cref="HeightsArray" />.
/// </summary>
type HeightsArrayResult = Result<HeightsArray option, string>