- hillshading
    - run the command on Alps

- update readme.md docs

- how to generate code docs?
    - https://fsprojects.github.io/FSharp.Formatting/

- in what format to save the shaded tile PNG metadata?  
    - extra sidecar file?
    - extra PNG chunk?

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