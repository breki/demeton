module Demeton.Projections.Factory

open Demeton.Projections
open PROJParsing

/// <summary>
/// Given a map projection type and map scale, creates a map projection
/// functions for it.
/// </summary>
let createMapProjection projection mapScale =
    match projection with
    | LambertConformalConic parameters ->
        LambertConformalConic.validateParameters parameters
        |> Result.map (fun () -> 
            LambertConformalConic.MapProjection(parameters, mapScale).projection)
    | Mercator ->
        Mercator.MapProjection(mapScale).projection
        |> Ok
