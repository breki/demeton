﻿- implement PNG writing
    - implement unfiltering of scanlines so we can write the property test
    - how do we implement scanline filtering?
    - prepare a test case for generating the simplest PNG
    - first grayscale (and with transparency, if possible)

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
    - produces what?
        - let's implement our own image type, for now
    - how to render it to a PNG?