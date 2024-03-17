module Demeton.Projections.Common

open Demeton.Geometry.Common

open System

[<Literal>]
let EarthRadiusInMeters = 6371000.

[<Literal>]
let MetersPerInch = 0.0254

[<Literal>]
let InchesPerMeter = 39.3701


[<Literal>]
let Epsilon = 1.0e-10

type Ellipsoid =
    { SemimajorRadius: float
      SemiminorRadius: float }

let GRS80 =
    { SemimajorRadius = 6378137.
      SemiminorRadius = 6356752.314140347 }

let WGS84 =
    { SemimajorRadius = 6378137.
      SemiminorRadius = 6356752.314245 }

let tryGetEllipsoid (ellipsoidId: string) =
    match ellipsoidId.ToLowerInvariant() with
    | "grs80" -> Ok GRS80
    | "wgs84" -> Ok WGS84
    | _ -> sprintf "Unsupported ellipsoid '%s'" ellipsoidId |> Error

let eccentricity ellipsoid =
    let ratio = ellipsoid.SemiminorRadius / ellipsoid.SemimajorRadius
    Math.Sqrt(1. - ratio * ratio)

//type Datum = {
//    DatumCode: string
//    DatumName: string
//    Ellipsoid: Ellipsoid
//}

type MapScale =
    { MapScale: float
      Dpi: float }

    member this.ProjectionScaleFactor =
        EarthRadiusInMeters / this.MapScale * InchesPerMeter * this.Dpi

    /// <summary>
    /// Represents map scale which has a projection scale factor of 1.
    /// </summary>
    static member ScaleOf1 =
        { MapScale = 1.
          Dpi = 1. / (EarthRadiusInMeters * InchesPerMeter) }

/// <summary>
/// Type alias for a function that represents a map projection.
/// </summary>
/// <param name="lon">
/// The longitude of the point in the geographical space (in radians).
/// </param>
/// <param name="lat">
/// The latitude of the point in the geographical space (in radians).
/// </param>
/// <returns>An option type that contains the x and y coordinates of the
/// point in the projected space if the projection is successful,
/// or None if the projection is not possible.</returns>
type ProjectFunc = float -> float -> Point option

/// <summary>
/// Type alias for a function that represents the inverse of a map projection.
/// </summary>
/// <param name="x">The x-coordinate of the point in the projected space.</param>
/// <param name="y">The y-coordinate of the point in the projected space.</param>
/// <returns>
/// An option type that contains the longitude and latitude (in radians) of the
/// point in the geographical space if the inversion is successful,
/// or None if the inversion is not possible.
/// </returns>
type InvertFunc = float -> float -> LonLat option

type MapProjection =
    { Proj: ProjectFunc
      Invert: InvertFunc }

/// <summary>
/// Calculates an approximate geodetic distance (in meters) between two points
/// on Earth. Not suitable for high-precision calculations.
/// </summary>
let geodeticDistanceApproximate lon1 lat1 lon2 lat2 =
    let dlat2 = (lat2 - lat1) / 2.
    let dlon2 = (lon2 - lon1) / 2.

    let a =
        Math.Sin dlat2 * Math.Sin dlat2
        + Math.Cos lat1 * Math.Cos lat2 * Math.Sin dlon2 * Math.Sin dlon2

    let c = 2. * Math.Atan2(Math.Sqrt a, Math.Sqrt(1. - a))
    EarthRadiusInMeters * c


/// <summary>
/// A PROJ parameter value.
/// </summary>
type PROJParameterValue =
    /// <summary>
    /// Value of a PROJ string parameter.
    /// </summary>
    | StringValue of string
    /// <summary>
    /// Value of a PROJ numeric parameter.
    /// </summary>
    | NumericValue of float

/// <summary>
/// A PROJ parameter.
/// </summary>
type PROJParameter =
    { Name: string
      Value: PROJParameterValue option }

/// <summary>
/// Tries to get a numeric value of the specified PROJ parameter. If the
/// parameter is not a numeric value, returns an error.
/// </summary>
let tryGetParameterNumericValue parameter =
    match parameter.Value with
    | Some(NumericValue value) -> Ok value
    | Some(StringValue _) ->
        sprintf "PROJ parameter '%s' must have a numeric value." parameter.Name
        |> Error
    | None ->
        sprintf "PROJ parameter '%s' is missing a value." parameter.Name
        |> Error

/// <summary>
/// Tries to get a string value of the specified PROJ parameter. If the
/// parameter is not a string value, returns an error.
/// </summary>
let tryGetParameterStringValue parameter =
    match parameter.Value with
    | Some(StringValue value) -> Ok value
    | Some(NumericValue value) ->
        value.ToString(System.Globalization.CultureInfo.InvariantCulture) |> Ok
    | None ->
        sprintf "PROJ parameter '%s' is missing a value." parameter.Name
        |> Error


let msfnz e sinphi cosphi =
    let con = e * sinphi
    cosphi / Math.Sqrt(1.0 - con * con)

let tsfnz e phi sinphi =
    let con = e * sinphi
    let com = e / 2.
    let con' = Math.Pow((1.0 - con) / (1.0 + con), com)
    tan (0.5 * (Math.PI / 2. - phi)) / con'

let adjustLon lon =
    if (abs (lon) < Math.PI) then
        lon
    else
        lon - ((sign lon |> float) * Math.PI * 2.)

/// <summary>
/// Determine latitude angle phi-2.
/// </summary>
let phi2z e ts =
    let eccnth = e / 2.

    let rec search i phi =
        if i > 15 then
            None
        else
            let con = e * Math.Sin(phi)

            let deltaPhi =
                Math.PI / 2.
                - 2. * atan (ts * Math.Pow((1. - con) / (1. + con), eccnth))
                - phi

            let phi' = phi + deltaPhi

            if abs deltaPhi < Epsilon then
                Some phi'
            else
                search (i + 1) phi'

    let startingPhi = Math.PI / 2. - 2. * atan ts
    search 0 startingPhi
