- more optimization of the slope function
- implement orientation calculation and merge tests into a single slope+orientation testing

- implement slope and aspect formulas
    - option 1: calculate normals of triangles
        https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal
        https://math.stackexchange.com/questions/305642/how-to-find-surface-normal-of-a-triangle
        https://en.wikipedia.org/wiki/Normal_(geometry)
        https://math.stackexchange.com/questions/608938/computing-a-normal-to-triangle
    - option 2: reuse existing Maperitive code

- hillshading
    - tests for calculating the slope, aspect, shade etc
    - implement the actual hillshader for the raster
    - integrate it into the shade command
- update readme.md docs
- how to generate code docs?
- make a more generic way of parsing command line parameters
    - preferably with the ability to auto-generate help

- in what format to save the shaded tile PNG metadata?  
    - extra sidecar file?
    - extra PNG chunk?
'
- we need one additional row of SRTM cells on each side to be able to calculate things for hillshading

- " To be more exact, these
coordinates refer to the geometric center of the lower left pixel, which in the case of SRTM3 data will be about 90 meters in extent."

- https://fsprojects.github.io/FSharp.Formatting/metadata.html

- proj4 links
    - https://en.wikibooks.org/wiki/PROJ.4
    - https://github.com/albertov/proj4hs
    - https://github.com/naqsha/naqsha
- hillshading links
    - http://www.saga-gis.org/saga_tool_doc/2.2.0/ta_lighting_0.html
    - https://www.rayshader.com/