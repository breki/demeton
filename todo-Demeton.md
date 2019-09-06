- implement PNG writing
    - chunk type should be a type
    - implement writing of chunks (+ CRC checks)
    - first grayscale (and with transparency, if possible)

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
    - produces what?
        - let's implement our own image type, for now
    - how to render it to a PNG?