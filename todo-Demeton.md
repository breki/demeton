- how do we test this in integration?
    - an acceptance test that actually fetches a sample level tile

- implement determineTileStatus
- implement convertFromHgt function
- implement createFromLowerTiles function
- how do we handle errors?

- SRTM tile loader should support higher tiles
    - also test negative scenarios and None heights arrays
- implement a proper resampler

- extend `decodeSrtmTileFromPngFile` to support levels

- generate GitHub releases: https://developer.github.com/v3/repos/releases/#create-a-release

- how to generate code docs?
    - https://fsprojects.github.io/FSharp.Formatting/commandline.html

- add batch script that generates some sample images from these sample DEMs

- investigate possible filters
    - use Paint.NET as a start
    
- hillshading
    - run the command on Alps

- update readme.md docs

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