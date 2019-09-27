- can we improve performance of HeightsArray <-> ImageData conversion by directly working on arrays?

- reorganize test files in a chronological order

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
