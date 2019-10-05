- ElevationColorScale
    - add support for None color for marks, too

- implement shaders
    - ElevationColoring
        - fetch the color ramps from Maperitive
    - RasterShader
    - support for elevation coloring and hillshading
- make a more generic way of parsing command line parameters
    - preferably with the ability to auto-generate help

- in what format to save the shaded tile PNG metadata?  
    - extra sidecar file?
    - extra PNG chunk?

- implement the actual hillshading algorithm
    - input: height array, tile rectangle, options
    - output: raw image data
    - for each pixel in tile:
        - fetch elevations of it and its neighbours
        - execute the shading algorithm
        - color the pixel

- we need one additional row of SRTM cells on each side to be able to calculate things for hillshading

- " To be more exact, these
coordinates refer to the geometric center of the lower left pixel, which in the case of SRTM3 data will be about 90 meters in extent."

- finish preparing a new shade command

- projection from lon,lat to WebMercator and inverse
    - take into account the Earth radius
    
    - map scale
    - DPI/PPI
    - projection center

- https://fsprojects.github.io/FSharp.Formatting/metadata.html

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm

- proj4 links
    - https://en.wikibooks.org/wiki/PROJ.4
    - https://github.com/albertov/proj4hs
    - https://github.com/naqsha/naqsha
- hillshading links
    - http://www.saga-gis.org/saga_tool_doc/2.2.0/ta_lighting_0.html
    - https://www.rayshader.com/