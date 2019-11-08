- implement some kind of search algorithm that identifies the minimum distance between pixels?
    - simulated annealing?
    - this function can work on lon, lat coordinates, SRTM coords are not needed
    - then, based on that, calculate the level needed
- implement support for multi-level DEM PNGs
    - implement a method that calculates the min (dx, dy) for a set of sample points
    - first implement a method that, for a given map scale and dpi, calculates the DEM level needed
    - for the lowest level, detect that DPI is too low 

- add sample DEM PNGs to the source control
- add batch script that generates some sample images from these sample DEMs

- investigate possible filters
    - use Paint.NET as a start
    
- hillshading
    - run the command on Alps

- update readme.md docs

- how to generate code docs?
    - https://fsprojects.github.io/FSharp.Formatting/

- in what format to save the shaded tile PNG metadata?  
    - extra sidecar file?
    - extra PNG chunk?

- we need one additional row of SRTM cells on each side to be able to calculate things for hillshading

- https://fsprojects.github.io/FSharp.Formatting/metadata.html

- proj4 links
    - https://en.wikibooks.org/wiki/PROJ.4
    - https://github.com/albertov/proj4hs
    - https://github.com/naqsha/naqsha
- hillshading links
    - http://www.saga-gis.org/saga_tool_doc/2.2.0/ta_lighting_0.html
    - https://www.rayshader.com/