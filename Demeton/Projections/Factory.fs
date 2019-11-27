module Demeton.Projections.Factory

open Demeton.Projections
open Parsing

/// <summary>
/// Given a map projection type and map scale, creates a map projection
/// functions for it.
/// </summary>
let createMapProjection projection mapScale =
    match projection with
    | Mercator -> Mercator.MapProjection(mapScale).projection
