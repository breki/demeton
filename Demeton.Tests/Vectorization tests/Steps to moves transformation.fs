module Tests.``Vectorization tests``.``Steps to moves transformation``

open Demeton.Vectorization.MarchingSquares
open Tests.``Vectorization tests``.``Isoline DSL``

open Xunit
open Swensen.Unquote

type Moves =
    | N of int
    | NE of int
    | E of int
    | SE of int
    | S of int
    | SW of int
    | W of int
    | NW of int

type IsolineMoves = {
    StartingPoint: Demeton.Geometry.Common.Point
    Moves: Moves list
}

let stepsToMoves (isoline: Isoline): IsolineMoves =
    invalidOp "todo"

let movesToSteps (moves: IsolineMoves): Isoline =
    invalidOp "todo"

[<Fact(Skip="todo")>]
let ``Transform simple square isoline steps into moves``() =
    let isoline = parseIsolineDef "o;v0,1;drul"
    
    test <@ stepsToMoves isoline =
                { StartingPoint = (0.5, 1.5);
                  Moves = [ SE 1; NE 1; NW 1; SW 1 ] } @>

