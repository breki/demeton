- check if there are any more optimizations/deletions possible

- profiling and optimizing the code
    - we should try using Spans to directly write into the image's array
        - we need Grayscale8BitImageData as 2d array or should we work with 1d byte array instead? - MEASURE!
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
