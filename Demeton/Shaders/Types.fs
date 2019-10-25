module Demeton.Shaders.Types

open Raster
open Demeton.DemTypes
open Demeton.Projections.Common

type ShaderOptions = {
    MapScale: float
    Dpi: float
    }

    with
    member this.ProjectionScaleFactor =
        EarthRadiusInMeters / this.MapScale * InchesPerMeter * this.Dpi
    
type RasterShader = 
    HeightsArray -> Raster.Rect -> RawImageData -> ShaderOptions -> unit

        