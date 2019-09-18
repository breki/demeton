62.06
58.78
52.19 ms

- possible PNG encoding performance improvements
    - keep the buffer in the same thread?
        - use a more advanced version of Parallel.For that initializes buffers and then gives them to the scanline filter function
    
    - separate filter code for the first line (when there is no previous scanline)
        - simple Array copy for None filter (instead of the for loop) and for the Up filter on the first scanline
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
