[<RequireQualifiedAccess>]
module Demeton.Projections.Mercator

open Demeton.Projections.Common

open System

[<Literal>]
let MaxLat = 1.48442222974533
[<Literal>]
let MinLat = -1.48442222974533

let proj: ProjectFunc = fun longitude latitude ->
    match latitude with
    | _ when latitude < MinLat || latitude > MaxLat -> None
    | _ -> 
        let x = longitude
        let y = Math.Log (Math.Tan (Math.PI / 4. + latitude / 2.))
        Some (x, y)

let inverse: InvertFunc = fun x y -> 
    let longitude = x
    let latitude = 2. * Math.Atan (Math.Exp y) - Math.PI / 2.
    Some (longitude, latitude)