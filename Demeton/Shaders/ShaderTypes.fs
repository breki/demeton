module Demeton.Shaders.ShaderTypes

type Shader = 
    ElevationColoringShader of ElevationColoring.ColorScale
    // todo: extend Hillshader with properties
    //| Hillshader of (PixelHillshader * ShaderParameters)
    | Hillshader