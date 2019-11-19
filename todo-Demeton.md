- heights array downsampling notes:
    - code: 
        - https://www.paulinternet.nl/?page=bicubic
    - https://en.wikipedia.org/wiki/Image_scaling
    - resampling
    - gridding, interpolation methods
    - methods used: 
        - nearest neighbour
        - bilinear
        - cubic convolution
        - "In general, bicubic interpolation works best for straight downsampling"
            - https://en.wikipedia.org/wiki/Bicubic_interpolation
    - grid resampling
    - decimation
    - 3x3 grid neighbourhood
    - Moore neighbourhood
    - kernel
    - https://en.wikipedia.org/wiki/Supersampling
    - low pass/high pass filters
        - Geospatial Analysis, p. 175

    - searches
        digital elevation model downsampling
        
    - articles:
        - https://www.geospatialworld.net/article/comparison-of-decimation-and-averaging-methods-of-dems-resampling/

- handle CreateFromLowerTiles errors in `processNextCommand`

- add batch script that generates some sample images from these sample DEMs

- hillshading
    - run the command on Alps

- Vectorization: 
    - https://en.wikipedia.org/wiki/Image_tracing
    - https://en.wikipedia.org/wiki/Color_quantization
    - https://en.wikipedia.org/wiki/Posterization

- generate GitHub releases: https://developer.github.com/v3/repos/releases/#create-a-release

- how to generate code docs?
    - https://fsprojects.github.io/FSharp.Formatting/commandline.html

- investigate possible filters
    - use Paint.NET as a start
    
- update readme.md docs

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