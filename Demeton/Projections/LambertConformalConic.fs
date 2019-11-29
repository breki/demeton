[<RequireQualifiedAccess>]
module Demeton.Projections.LambertConformalConic

// https://proj.org/operations/projections/lcc.html

open Demeton.Geometry.Common
open Demeton.Projections.Common

open System

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

/// <summary>
/// Tries to get a numeric value of the specified PROJ parameter. If the
/// parameter is not a numeric value, returns an error.
/// </summary>
let tryGetParameterNumericValue parameter =
    match parameter.Value with
    | Some (NumericValue value) -> Ok value
    | Some (StringValue _) -> 
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
    | Some (StringValue value) -> Ok value
    | Some (NumericValue value) -> 
        value.ToString (System.Globalization.CultureInfo.InvariantCulture)
        |> Ok 
    | None -> 
        sprintf "PROJ parameter '%s' is missing a value." parameter.Name
        |> Error

let tryGetEllipsoid (ellipsoidId: string) =    
    match ellipsoidId.ToLowerInvariant() with
    | "grs80" -> Ok GRS80
    | "wgs84" -> Ok WGS84
    | _ ->
        sprintf "Unsupported ellipsoid '%s'" ellipsoidId
        |> Error
    
let extractParameters projParameters:
    Result<(Parameters * PROJParameter list), string> =
    
    let processParameter resultSoFar parameter =
        resultSoFar 
        |> Result.bind (fun (parameters, ignoredParameters) -> 
            match parameter.Name with
            | "lon_0" -> 
                tryGetParameterNumericValue parameter
                |> Result.map (fun value -> 
                    { parameters with Lon0 = value }, ignoredParameters)
            | "lat_0" -> 
                tryGetParameterNumericValue parameter
                |> Result.map (fun value -> 
                    { parameters with Lat0 = value }, ignoredParameters)
            | "lat_1" -> 
                tryGetParameterNumericValue parameter
                |> Result.map (fun value -> 
                    { parameters with Lat1 = value }, ignoredParameters)
            | "lat_2" -> 
                tryGetParameterNumericValue parameter
                |> Result.map (fun value -> 
                    { parameters with Lat2 = value }, ignoredParameters)
            | "x_0" -> 
                tryGetParameterNumericValue parameter
                |> Result.map (fun value -> 
                    { parameters with X0 = value }, ignoredParameters)
            | "y_0" -> 
                tryGetParameterNumericValue parameter
                |> Result.map (fun value -> 
                    { parameters with Y0 = value }, ignoredParameters)
            | "k_0" -> 
                tryGetParameterNumericValue parameter
                |> Result.map (fun value -> 
                    { parameters with K0 = value }, ignoredParameters)
            | "ellps" ->
                tryGetParameterStringValue parameter
                |> Result.bind tryGetEllipsoid
                |> Result.map (fun value -> 
                    { parameters with Ellipsoid = value }, ignoredParameters)
            | _ -> Ok (parameters, parameter :: ignoredParameters)
                )
            
    let defaultParameters = {
        X0 = 0.; Y0 = 0. 
        Lon0 = 0.; Lat0 = 0.; Lat1 = 0.; Lat2 = 0.; K0 = 1.
        Ellipsoid = GRS80 }
            
    projParameters
    |> List.fold processParameter (Result.Ok (defaultParameters, []))

let msfnz e sinphi cosphi =
    let con = e * sinphi
    cosphi / Math.Sqrt (1.0 - con * con)

let tsfnz e phi sinphi =
    let con = e * sinphi
    let com = e / 2.
    let con' = Math.Pow ((1.0 - con) / (1.0 + con), com)
    tan (0.5 * (Math.PI / 2. - phi)) / con';

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
        if i > 15 then None
        else
            let con = e * Math.Sin (phi)
            let deltaPhi =
                Math.PI / 2.
                - 2. * atan (ts * Math.Pow ((1. - con) / (1. + con), eccnth))
                - phi
            let phi' = phi + deltaPhi
            
            if abs deltaPhi < Epsilon then Some phi'
            else search (i + 1) phi'
            
    let startingPhi = Math.PI / 2. - 2. * atan ts
    search 0 startingPhi      

let validateParameters parameters =
     if Math.Abs (parameters.Lat1 + parameters.Lat2) < Epsilon then
        "Standard parallels cannot be equal or on opposite sides of the equator."
        |> Error
     else Ok()

type MapProjection(parameters: Parameters, mapScale: MapScale) =
    let scaleFactor = InchesPerMeter * mapScale.Dpi  / mapScale.MapScale 

    let x0 = parameters.X0
    let y0 = parameters.Y0
    
    let lon0 = parameters.Lon0 |> degToRad
    let lat0 = parameters.Lat0 |> degToRad
    let lat1 = parameters.Lat1 |> degToRad
    let lat2 = parameters.Lat2 |> degToRad
    let k0 = parameters.K0
    
    let semimajor = parameters.Ellipsoid.SemimajorRadius
    let e = eccentricity parameters.Ellipsoid
    
    let sin1 = sin lat1
    let cos1 = cos lat1
    let ms1 = msfnz e sin1 cos1
    let ts1 = tsfnz e lat1 sin1
    
    let sin2 = sin lat2
    let cos2 = cos lat2
    let ms2 = msfnz e sin2 cos2
    let ts2 = tsfnz e lat2 sin2
        
    let ts0 = tsfnz e lat0 (sin lat0)

    let ns =
        if abs (lat1 - lat2) > Epsilon then
            Math.Log (ms1 / ms2) / Math.Log (ts1 / ts2)
        else
            sin1
        
    let f0 = ms1 / (ns * Math.Pow (ts1, ns))
    let rh = parameters.Ellipsoid.SemimajorRadius * f0 * Math.Pow (ts0, ns)
            
    do
        match validateParameters parameters with
        | Result.Ok() -> ignore()
        | Result.Error message ->
            invalidArg "parameters" message
            
    member this.proj: ProjectFunc = fun longitude latitude ->
        let con = Math.Abs (Math.Abs (latitude) - (Math.PI / 2.))

        if con > Epsilon then
            let ts = tsfnz e latitude (sin latitude)
            let rh1 = semimajor * f0 * Math.Pow (ts, ns)
            let theta = ns * adjustLon(longitude - lon0)
            let x = k0 * (rh1 * sin theta) + x0
            let y = k0 * (rh - rh1 * cos theta) + y0
            Some (x * scaleFactor, y * scaleFactor)
        else
            if latitude * ns <= 0. then None
            else
                // rh1 = 0
                let x = k0 + x0
                let y = k0 * rh + y0
                Some (x * scaleFactor, y * scaleFactor)
    
    member this.inverse: InvertFunc = fun x y ->
        let x' = ((x / scaleFactor) - x0) / k0
        let y' = rh - ((y / scaleFactor) - y0) / k0;
        
        let sign = if ns > 0. then 1. else -1.
        
        let rh1 = sign * Math.Sqrt (x' * x' + y' * y')
        let con = sign

        let theta = 
            match rh1 with
            | 0. -> 0.
            | _ -> Math.Atan2 (con * x', con * y')
            
        if rh1 <> 0. || ns > 0. then
            let con' = 1. / ns
            let ts = Math.Pow (rh1 / (semimajor * f0), con')
            let latMaybe = phi2z e ts
            match latMaybe with
            | None -> None
            | Some lat ->
                let lon = adjustLon (theta / ns + lon0)
                Some (lon, lat)
        else
            let lat = -Math.PI / 2.
            let lon = adjustLon (theta / ns + lon0)
            Some (lon, lat)
    
    member this.projection =
        { Proj = this.proj; Invert = this.inverse }
