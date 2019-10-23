module Demeton.Shaders.ShaderTypes

open ElevationColoring

type Shader = 
    ElevationColoringShader of ElevationColorScale
    // todo: extend Hillshader with properties
    //| Hillshader of (PixelHillshader * ShaderParameters)
    | Hillshader