- the whole thing is just too damn slow, how do we speed it up?
    - main filter method should write the filter type byte instead of letting the filter type function do it
    - parallelize things?

- update the filter docs to reflect the latest changes

- implement converter of HGT files into PNG
    - also implement decoder back and write property tests

- implement PNG writing
    - add support for RGBA

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
