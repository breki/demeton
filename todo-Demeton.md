- something is wrong with the updated SRTM importing code
    - write tests that sample exact heights for a given cell
    - the function that reads SRTM heights should skip the +1 height so we can implement a simpler initialization of HeightsArray

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
