- implement PNG writing
    - http://plinth.org/techtalk/?p=196
    - implement writing of chunks (+ CRC checks)
    - first grayscale (and with transparency, if possible)

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
    - produces what?
        - let's implement our own image type, for now
    - how to render it to a PNG?