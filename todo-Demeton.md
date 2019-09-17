- replace 2D arrays with 1D ones
    - instead of 2D arrays, provide functions for working with them

- profiling and optimizing the code
    - speed up decoding of PNG images
    - speed up CRC processing
        - direct encoding instead of recursion/loop?
    - implement decoding the SRTM cell from PNG image
        - it's too slow right now

- update the filter docs to reflect the latest changes

- implement converter of HGT files into PNG
    - also implement decoder back and write property tests

- implement PNG writing
    - add support for RGBA

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
