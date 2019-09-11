- implement PNG writing
    - implement support for 16-bit grayscale images
        - `serializeIdatChunkData` and `deserializeIdatChunkData` should have
            bpp value
        - also, the filtering functions should work with bpp
    - reading should implement some kind of routing for chunks
    - document `Demeton.Png` and `Demeton.PngFilters` modules
    - first grayscale (and with transparency, if possible)

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
    - produces what?
        - let's implement our own image type, for now
    - how to render it to a PNG?