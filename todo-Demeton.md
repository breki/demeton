- add batch script that generates some sample images from these sample DEMs

- update readme.md docs
- add documentation for
    - releases
        https://github.com/breki/demeton/releases/latest
        - `libgdiplus` package is needed on Linux
    - 2-min tutorial
    - 5-min tutorial

- hillshading

- Vectorization: 
    - https://en.wikipedia.org/wiki/Image_tracing
    - https://en.wikipedia.org/wiki/Color_quantization
    - https://en.wikipedia.org/wiki/Posterization

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