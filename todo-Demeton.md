- implement PNG writing
    - implement compression
        - deflate/inflate compression with a sliding window (which is an upper bound on the distances appearing in the deflate stream) of at most 32768 bytes. 
        - Deflate compression is an LZ77 derivative
        - Deflate-compressed datastreams within PNG are stored in the "zlib" format, which has the structure:
        - the zlib compression method/flags code shall specify method code 8 (deflate compression) and an LZ77 window size of not more than 32768 bytes
        - The additional flags shall not specify a preset dictionary.
        - If the data to be compressed contain 16384 bytes or fewer, the PNG encoder may set the window size by rounding up to a power of 2 (256 minimum). This decreases the memory required for both encoding and decoding, without adversely affecting the compression ratio.


    - document `Demeton.Png` and `Demeton.PngFilters` modules
    - property test that generates a PNG from a data array and then decodes it back
    - prepare a test case for generating the simplest PNG
    - first grayscale (and with transparency, if possible)

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
    - produces what?
        - let's implement our own image type, for now
    - how to render it to a PNG?