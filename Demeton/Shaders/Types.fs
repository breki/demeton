module Demeton.Shaders.Types

open Raster
open Demeton.DemTypes
open Demeton.Projections.Common
open Demeton.Srtm.Types

/// <summary>
/// A function that takes a list of heights arrays, a SRTM level,
/// a rectangle, a raw image data and a map projection forward and inverse
/// functions and applies a shader to the image.
/// </summary>
type RasterShader =
    HeightsArray[]
        -> SrtmLevel
        -> Rect
        -> RawImageData
        -> ProjectFunc
        -> InvertFunc
        -> unit
