- clean up namespaces/module names a little
    - separate generic DEM code from SRTM
    - extract tile-oriented SRTM funcs to a separate module
    - restructure SRTM-oriented test code into a separate folder

- try to put all the pieces together so that the command is running
    - parallelize the importing process
        - fix logging so it can work in parallel mode

- https://fsprojects.github.io/FSharp.Formatting/metadata.html
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
