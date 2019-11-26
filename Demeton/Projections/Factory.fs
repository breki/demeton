module Demeton.Projections.Factory

open Demeton.Projections
open Parsing

/// <summary>
/// Given a map projection, returns its twin projection functions.
/// </summary>
let prepareProjectionFunctions projection =
    match projection with
    | Mercator -> (Mercator.proj, Mercator.inverse)
