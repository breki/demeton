module Demeton.DemTypes 

type DemHeight = int16

[<Literal>]
let DemHeightNone = System.Int16.MinValue

let inline DemHeight x = int16 x

type GlobalCellCoords = (int * int)

type HeightCell = { Coords: GlobalCellCoords; Height : DemHeight }

type HeightsArrayInitializer =
    HeightsArrayInitializer1D of (int -> DemHeight)
    | HeightsArrayInitializer2D of (GlobalCellCoords -> DemHeight)

type HeightsArray
    (
        minX: int,
        minY: int,
        width: int, 
        height: int,
        initializer: HeightsArrayInitializer) =
    let cells =
        let arraySize = width*height

        let initializerFuncToUse = 
            match initializer with
            | HeightsArrayInitializer2D initializer2D ->
                    fun index -> 
                        let x = index % width
                        let y = index / width
                        initializer2D (minX + x, minY + y)
            | HeightsArrayInitializer1D initializer1D -> initializer1D

        Array.init<DemHeight> arraySize initializerFuncToUse

    member this.MinX = minX
    member this.MinY = minY
    member this.Width = width
    member this.Height = height
    member this.MaxX = minX + width - 1
    member this.MaxY = minY + height - 1
    member this.Cells = cells
    
    member this.heightAt ((x, y): GlobalCellCoords) = 
        let index = (y - this.MinY) * width + x - this.MinX
        let heightAtCell = this.Cells.[index]
        heightAtCell
        
/// <summary>
/// A result of an operation that can return an optional 
/// <see cref="HeightsArray" />.
/// </summary>
type HeightsArrayResult = Result<HeightsArray option, string>