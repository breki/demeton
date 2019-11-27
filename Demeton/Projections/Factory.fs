module Demeton.Projections.Factory

open Demeton.Projections
open Demeton.Projections.Common
open Parsing
open System

type MapProjection = { Proj: ProjectFunc; Invert: InvertFunc }

type MercatorMapProjection() =
    [<Literal>]
    let MaxLat = 1.48442222974533
    [<Literal>]
    let MinLat = -1.48442222974533

    member this.proj: ProjectFunc = fun longitude latitude ->
        match latitude with
        | _ when latitude < MinLat || latitude > MaxLat -> None
        | _ -> 
            let x = longitude
            let y = Math.Log (Math.Tan (Math.PI / 4. + latitude / 2.))
            Some (x, y)

    member this.inverse: InvertFunc = fun x y -> 
        let longitude = x
        let latitude = 2. * Math.Atan (Math.Exp y) - Math.PI / 2.
        Some (longitude, latitude)
    
    member this.projection =
        { Proj = this.proj; Invert = this.inverse }

/// <summary>
/// Given a map projection, returns its twin projection functions.
/// </summary>
let prepareProjectionFunctions projection =
    match projection with
    | Mercator -> MercatorMapProjection().projection
