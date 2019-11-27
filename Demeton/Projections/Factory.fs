module Demeton.Projections.Factory

open Demeton.Projections
open Demeton.Projections.Common
open Parsing

type MapProjection = { Proj: ProjectFunc; Invert: InvertFunc }

/// <summary>
/// Given a map projection, returns its twin projection functions.
/// </summary>
let prepareProjectionFunctions projection =
    match projection with
    | Mercator -> { Proj = Mercator.proj; Invert = Mercator.inverse }
