module Demeton.Shaders.Types

open Raster
open Demeton.DemTypes
open Demeton.Projections.Common
open Demeton.Srtm.Types
    
type RasterShader = 
    HeightsArray -> SrtmLevel -> Raster.Rect -> RawImageData -> InvertFunc
        -> MapScale -> unit

        