module Demeton.DemTypes 

type DemHeight = int16

[<Literal>]
let DemHeightNone = System.Int16.MinValue

let interpolateHeight h1 h2 h3 h4 (dx: float) (dy: float) =
    if h1 = DemHeightNone || h2 = DemHeightNone
        || h3 = DemHeightNone || h4 = DemHeightNone then
        None
    else
        let hh1 = float (h2 - h1) * dx + float h1
        let hh2 = float (h4 - h3) * dx + float h3
        let height = float (hh2 - hh1) * dy + hh1
        Some height

let inline DemHeight x = int16 x

type GlobalCellCoords = (int * int)
type GlobalCellCoordsFractional = (float * float)

type HeightCell = { Coords: GlobalCellCoords; Height : DemHeight }

type HeightsArrayInitializer =
    HeightsArrayDirectImport of (DemHeight[])
    | HeightsArrayInitializer1D of (int -> DemHeight)
    | HeightsArrayInitializer2D of (GlobalCellCoords -> DemHeight)
    | HeightsArrayCustomInitializer of (DemHeight[] -> unit)

type HeightsArray
    (
        minX: int,
        minY: int,
        width: int, 
        height: int,
        initializer: HeightsArrayInitializer) =
    let cells =
        let arraySize = width*height

        match initializer with
        | HeightsArrayDirectImport arrayToImport ->
            if arrayToImport.Length <> arraySize then
                invalidOp "The imported heights array is of incompatible size."

            arrayToImport
        | HeightsArrayInitializer2D initializer2D ->
            let initializerFuncToUse =
                fun index -> 
                    let x = index % width
                    let y = index / width
                    initializer2D (minX + x, minY + y)
            Array.init<DemHeight> arraySize initializerFuncToUse
        | HeightsArrayInitializer1D initializer1D -> 
            Array.init<DemHeight> arraySize initializer1D
        | HeightsArrayCustomInitializer customInitializer ->
            let cellsToInitialize = Array.zeroCreate<DemHeight> arraySize
            customInitializer cellsToInitialize
            cellsToInitialize


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

    member this.interpolateHeightAt ((x, y): float * float) =
        let fractionOf value = value - floor value

        let x1 = int (floor x)
        let x2 = int (ceil x)
        let y1 = int (floor y)
        let y2 = int (ceil y)

        let h1 = this.heightAt (x1, y1)
        let h2 = this.heightAt (x2, y1)
        let h3 = this.heightAt (x1, y2)
        let h4 = this.heightAt (x2, y2)
        interpolateHeight h1 h2 h3 h4 (fractionOf x) (fractionOf y)

    member this.setHeightAt ((x, y): GlobalCellCoords) (height:  DemHeight) =
        let index = (y - this.MinY) * width + x - this.MinX
        this.Cells.[index] <- height
        
        
/// <summary>
/// A result of an operation that can return an optional 
/// <see cref="HeightsArray" />.
/// </summary>
type HeightsArrayResult = Result<HeightsArray option, string>