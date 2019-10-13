module Demeton.Projections.Common

open System

[<Literal>]
let EarthRadiusInMeters = 6371000.

[<Literal>]
let MetersPerInch = 0.0254

[<Literal>]
let InchesPerMeter = 39.3701

/// <summary>
/// Calculates an approximate geodetic distance (in meters) between two points 
/// on Earth. Not suitable for high-precision calculations.
/// </summary>
let geodeticDistanceApproximate lon1 lat1 lon2 lat2 =
    let dlat2 = (lat2 - lat1) / 2.
    let dlon2 = (lon2 - lon1) / 2.

    let a = 
        Math.Sin dlat2 * Math.Sin dlat2
        + Math.Cos lat1 * Math.Cos lat2
        * Math.Sin dlon2 * Math.Sin dlon2
    let c = 2. * Math.Atan2(Math.Sqrt a, Math.Sqrt(1.-a))
    EarthRadiusInMeters * c
