- github migration
    - remove project from bitbucket

- implement PNG writing
    - implement unfiltering of scanlines so we can write the property test
        - implement a generator for an array of scanlines with the same width
    - define type aliases for scanline and filtered scanline
    - extract filter selection as an extra method
    - move the PNG code to the main assembly
    - property test that generates a PNG from a data array and then decodes it back
    - prepare a test case for generating the simplest PNG
    - first grayscale (and with transparency, if possible)

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
    - produces what?
        - let's implement our own image type, for now
    - how to render it to a PNG?