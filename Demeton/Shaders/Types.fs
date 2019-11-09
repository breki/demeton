module Demeton.Shaders.Types

open Raster
open Demeton.DemTypes
open Demeton.Projections.Common
    
type RasterShader = 
    HeightsArray -> Raster.Rect -> RawImageData -> MapProjectionParameters -> unit

        