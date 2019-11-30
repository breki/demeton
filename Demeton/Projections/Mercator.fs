[<RequireQualifiedAccess>]
module Demeton.Projections.Mercator

// https://proj.org/operations/projections/merc.html

open Demeton.Projections.Common

open System

[<Literal>]
let MaxLat = 1.48442222974533
[<Literal>]
let MinLat = -1.48442222974533


type Parameters = {
    /// <summary>
    /// False easting.
    /// </summary>
    X0: float
    /// <summary>
    /// False northing.
    /// </summary>
    Y0: float
    /// <summary>
    /// Longitude of projection center.
    /// </summary>
    Lon0: float
    /// <summary>
    /// Latitude of projection center.
    /// </summary>
    Lat0: float
    /// <summary>
    /// First standard parallel.
    /// </summary>
    Lat1: float
    /// <summary>
    /// Second standard parallel.
    /// </summary>
    Lat2: float
    
    /// <summary>
    /// This parameter can represent two different values depending on the form
    /// of the projection. In LCC 1SP it determines the scale factor at natural
    /// origin. In LCC 2SP Michigan it determines the ellipsoid scale factor.
    /// </summary>
    K0: float
    
    Ellipsoid: Ellipsoid
}

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
