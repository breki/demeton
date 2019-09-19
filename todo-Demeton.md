- profile and speed up PNG decoding 
    - can we improve the unfilter types code?

- possible PNG encoding performance improvements
    - split IDAT into several chunks that can be CRC-processed in parallel

- profiling and optimizing the code
    - profile PNG encoding 
    - implement decoding the SRTM cell from PNG image
        - it's too slow right now
    - can we try different deflate strategies (see the [document](http://optipng.sourceforge.net/pngtech/optipng.html))?

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
