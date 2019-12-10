module Demeton.Geometry.Funcs

open TolerantMath

let area2 (x1, y1) (x2, y2) (x3, y3): float =
    (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)

type LeftResult = Left | Collinear | Right

let left tolerance area2 =
    match area2 with
    | x when isZero tolerance x -> Collinear
    | x when x > 0. -> Left
    | x when x < 0. -> Right
    | _ -> invalidOp "bug: this should never happen"

let between (left: LeftResult) (x1, y1) (x2, y2) (x3, y3) =
    if left <> Collinear then false
    else
        // if p1-p2 is not vertical, check betweenness on X
        if x1 <> x2 then
            (x1 <= x3 && x3 <= x2) || (x1 >= x3 && x3 >= x2) 
        // else on Y
        else
            (y1 <= y3 && y3 <= y2) || (y1 >= y3 && y3 >= y2)
