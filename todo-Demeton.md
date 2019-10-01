
- " To be more exact, these
coordinates refer to the geometric center of the lower left pixel, which in the case of SRTM3 data will be about 90 meters in extent."

- finish preparing a new shade command

- projection from lon,lat to WebMercator and inverse
    - take into account the Earth radius
    
    - map scale
    - DPI/PPI
    - projection center

- https://fsprojects.github.io/FSharp.Formatting/metadata.html

- hillshading algorithm
    - takes the `HeightsArray` and executes the Igor's shading algorithm
