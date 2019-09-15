- add benchmarking tests

- profiling and optimizing the code
    - speed up decoding of PNG images
        - unfilter functions should not use array comprehensions
            - we should try using Spans to directly write into the image's array
    - speed up CRC processing
    - implement decoding the SRTM cell from PNG image
        - it's too slow right now
    - use Memory and Span vs ordinary arrays?

- the whole thing is just too damn slow, how do we speed it up?
    - reuse filtered scanline arrays instead of recreating them?
    - alternative implementation:
        - one filter method that picks up left, up etc. and then calculates the filter value for each filter type
        - in the end it copies the best filter to a new filtered scanline that is returned
    - parallelize things?

- update the filter docs to reflect the latest changes

- implement converter of HGT files into PNG
    - also implement decoder back and write property tests

- implement PNG writing
    - add support for RGBA

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
