module Demeton.Shaders.ShaderTypes

open ElevationColoring

type Shader = 
    ElevationColoringShader of ElevationColorScale
    //| Hillshader of (PixelHillshader * ShaderParameters)
    | Hillshader