﻿- rename IhdrChunk to IhdrData
- start documenting stuff
- implement PNG writing
    - prepare a test case for generating the simplest PNG
    - `writeIhdrChunkData` should use the `ChunkDataWriter` signature
    - http://plinth.org/techtalk/?p=196
    - first grayscale (and with transparency, if possible)

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
    - produces what?
        - let's implement our own image type, for now
    - how to render it to a PNG?