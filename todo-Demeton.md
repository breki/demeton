- add support for both elevation coloring and hillshading in the `shade` command
    - how to specify shading parameters?
    - how to specify which shader to use?
    - how to combine shaders in one image?

- ShadeCommand.shadeRaster
    - write tests

- hillshading
    - implement the actual hillshader for the raster
    - implement Igor's shading method
    - integrate it into the shade command
- update readme.md docs

- how to generate code docs?
    - https://fsprojects.github.io/FSharp.Formatting/

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