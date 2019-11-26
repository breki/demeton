- tutorial
    - describe elevation scale parameters
    - slope
    - tiling
        - split into small tiles and add them as demo
- publish official release (1.0.0.0)

- add documentation for
    - 10-min tutorial
    - building
    - architecture

- in what format to save the shaded tile PNG metadata?  
    - JSON file convering all of the tiles
        - contains projection information (PROJ)
        - pixel coordinates of the top left corner  
        - also lon/lat coordinates of the top left corner

- image tracing: http://potrace.sourceforge.net/#dual

- generate some nice samples for the documentation
    - Mount Fuji: 138.637436,35.288563,138.822693,35.434156
    - Kilimanjaro: 36.933563,-3.416918,37.721558,-2.825636
    - Etna
    - add batch script that generates some sample images from these sample DEMs

- how to generate code docs?
    - https://fsprojects.github.io/FSharp.Formatting/commandline.html

- investigate possible filters
    - low pass/high pass filters
        - Geospatial Analysis, p. 175
    - use Paint.NET as a start

- proj4 links
    - https://en.wikibooks.org/wiki/PROJ.4
    - https://github.com/albertov/proj4hs
    - https://github.com/naqsha/naqsha
- hillshading links
    - http://www.saga-gis.org/saga_tool_doc/2.2.0/ta_lighting_0.html
    - https://www.rayshader.com/