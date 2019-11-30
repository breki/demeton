[<RequireQualifiedAccess>]
module Demeton.Projections.Mercator

open Demeton.Projections.Common

open System

[<Literal>]
let MaxLat = 1.48442222974533
[<Literal>]
let MinLat = -1.48442222974533

// todo: Mercator projection should be extended with its PROJ parameters
// (see https://proj.org/operations/projections/merc.html)
// Currently it uses true scale on the Equator and it ignores datum/ellipsoid.
type MapProjection(mapScale: MapScale) =
    let mapScale = mapScale
    let scaleFactor = mapScale.ProjectionScaleFactor 
        
    member this.proj: ProjectFunc = fun longitude latitude ->
        match latitude with
        | _ when latitude < MinLat || latitude > MaxLat -> None
        | _ -> 
            let x = longitude * scaleFactor
            let y =
                Math.Log (Math.Tan (Math.PI / 4. + latitude / 2.))
                    * scaleFactor
            Some (x, y)

    member this.inverse: InvertFunc = fun x y -> 
        let longitude = x / scaleFactor
        let latitude =
            2. * Math.Atan (Math.Exp (y / scaleFactor)) - Math.PI / 2.
        Some (longitude, latitude)
    
    member this.projection =
        { Proj = this.proj; Invert = this.inverse }
