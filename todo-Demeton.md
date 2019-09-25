- logging
    - how do we expose logging?
    - fix problems with multithreading logging
    - logging time?

- fix todos if possible
- remove code that's no longer used (look into local cache functions)

- DEM performance improvements
    - switch to using 1D array instead of 2D for heights (for performance reasons)
    - stop using an optional height in the HeightsArray (for performance reasons)

- https://fsprojects.github.io/FSharp.Formatting/metadata.html

- possible PNG encoding performance improvements
    - split IDAT into several chunks that can be CRC-processed in parallel

- profiling and optimizing the code
    - profile PNG encoding 
    - implement decoding the SRTM cell from PNG image

- go through the PNG code:
    - fix any todos
    - clean it up
    - reorganize it
    - update the docs

- add support for RGBA

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
