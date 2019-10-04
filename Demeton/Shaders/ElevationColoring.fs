module Demeton.Shaders.ElevationColoring

open Demeton.DemTypes
open Png

type ElevationColorer = DemHeight -> Rgba8Bit.RgbaColor option