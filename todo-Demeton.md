- Include license file in the distribution.

- generate some nice samples for the documentation
    - Alps
    - Mount Fuji: 138.637436,35.288563,138.822693,35.434156
    - Kilimanjaro: 36.933563,-3.416918,37.721558,-2.825636
    - Etna
- update readme.md docs
- add batch script that generates some sample images from these sample DEMs

- add documentation for
    - releases
        https://github.com/breki/demeton/releases/latest
        - `libgdiplus` package is needed on Linux
    - 2-min tutorial
    - 5-min tutorial

- how to generate code docs?
    - https://fsprojects.github.io/FSharp.Formatting/commandline.html

- investigate possible filters
    - low pass/high pass filters
        - Geospatial Analysis, p. 175
    - use Paint.NET as a start

- in what format to save the shaded tile PNG metadata?  
    - extra sidecar file?
    - extra PNG chunk?

- proj4 links
    - https://en.wikibooks.org/wiki/PROJ.4
    - https://github.com/albertov/proj4hs
    - https://github.com/naqsha/naqsha
- hillshading links
    - http://www.saga-gis.org/saga_tool_doc/2.2.0/ta_lighting_0.html
    - https://www.rayshader.com/