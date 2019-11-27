module Demeton.Projections.Factory

open Demeton.Projections
open Demeton.Projections.Common
open Parsing
open System

type MapProjection = { Proj: ProjectFunc; Invert: InvertFunc }

type MercatorMapProjection(mapScale: MapScale) =
    [<Literal>]
    let MaxLat = 1.48442222974533
    [<Literal>]
    let MinLat = -1.48442222974533

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

/// <summary>
/// Given a map projection type and map scale, creates a map projection object.
/// </summary>
let createMapProjection projection mapScale =
    match projection with
    | Mercator -> MercatorMapProjection(mapScale).projection
