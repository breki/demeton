- introduce FParsec for parsing the command line

- prepare a new command to generate hillshading

- projection from lon,lat to WebMercator and inverse
    - take into account the Earth radius
    
    - map scale
    - DPI/PPI
    - projection center

- https://fsprojects.github.io/FSharp.Formatting/metadata.html

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
