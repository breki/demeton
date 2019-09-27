- can we improve performance of HeightsArray <-> ImageData conversion by directly working on arrays?
    - `decodeSrtmTileFromPngFile`.`generateHeightsArray` could benefit from HeightsArray cell data being directly initialized

- reorganize test files in a chronological order

- https://fsprojects.github.io/FSharp.Formatting/metadata.html

- profiling and optimizing the code
    - profile PNG encoding 
    - profile PNG decoding

- add support for RGBA

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
