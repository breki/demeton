﻿[<RequireQualifiedAccess>]
module Demeton.Projections.WebMercator

open System

[<Literal>]
let MaxLat = 85.051128779806589
[<Literal>]
let MinLat = -85.051128779806589

let degToRad deg = deg * Math.PI / 180.
let radToDeg rad = rad * 180. / Math.PI

let proj longitude latitude =
    match latitude with
    | _ when latitude < MinLat || latitude > MaxLat -> None
    | _ -> 
        let x = degToRad longitude
        let y = Math.Log (Math.Tan (Math.PI * (0.25 + latitude / 360.)))
        Some (x, y)

let inverse x y = 
    let longitude = radToDeg x
    let latitude = 
        (Math.Atan (Math.Exp y) / Math.PI - 0.25) * 360.;
    Some (longitude, latitude)