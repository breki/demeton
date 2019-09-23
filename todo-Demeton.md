- define a function that imports a single SRTM tile
    - whole workflow -> check if PNG exists -> unzip, read HGT, encode to PNG, delete HGT

- command line
    - command line help text

- possible PNG encoding performance improvements
    - split IDAT into several chunks that can be CRC-processed in parallel

- profiling and optimizing the code
    - profile PNG encoding 
    - implement decoding the SRTM cell from PNG image

- implement console commands for encoding and decoding HGTS into PNGS so we can see the real speed (without debugging)
    - https://github.com/commandlineparser/commandline

- go through the PNG code:
    - fix any todos
    - clean it up
    - reorganize it
    - update the docs

- add support for RGBA

- switch to using 1D array instead of 2D for heights (for performance reasons)
- stop using an optional height in the HeightsArray (for performance reasons)

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
