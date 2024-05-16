module Demeton.Shaders.Types

open Raster
open Demeton.Dem.Types
open Demeton.Projections.Common

/// <summary>
/// A function that takes a list of heights arrays, a SRTM level,
/// a rectangle, a raw image data and a map projection forward and inverse
/// functions and applies a shader to the image.
/// </summary>
type RasterShader =
    HeightsArray[]
        -> DemLevel
        -> Rect
        -> RawImageData
        -> ProjectFunc
        -> InvertFunc
        -> unit
