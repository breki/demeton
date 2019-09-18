- move unfiltering code into a separate file

- possible PNG encoding performance improvements
    - split IDAT into several chunks that can be CRC-processed in parallel

- profiling and optimizing the code
    - profile PNG encoding 
    - profile and speed up PNG decoding 
        - test on a larger image
        - one option would be to skip CRC checks
    - implement decoding the SRTM cell from PNG image
        - it's too slow right now

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
