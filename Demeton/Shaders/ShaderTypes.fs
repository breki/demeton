module Demeton.Shaders.ShaderTypes

open ElevationColoring
open Hillshading

type Shader = 
    ElevationColoringShader of ElevationColorScale
    | Hillshader of (PixelHillshader * ShaderParameters)
    | NewHillshader