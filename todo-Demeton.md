- implement a binary search tree
    - first implement a testbed
        - property function that uses a list as a test oracle and also calls insert and delete functions on the BST
        - test case function that creates generators
            - generate a random sequence of insert and remove calls (with greater frequency of insert)
            - insert an integer
            - remove one of the items from the tree
                - how do we generate a random value between 1 and the tree size?
        - we also need to validate behavior of remove when the element does not exist

- run Europe on LCC
    - Could not open SRTM HTG file 'G:\SRTM\N44E048.SRTMGL1.hgt.zip': ZIP file 'G:\SRTM\N44E048.SRTMGL1.hgt.zip' does not have a file entry 'N44E048.hgt'.


- report ignored PROJ parameters in the console

- add documentation for
    - building
    - architecture

- in what format to save the shaded tile PNG metadata?  
    - JSON file convering all of the tiles
        - contains projection information (PROJ)
        - pixel coordinates of the top left corner  
        - also lon/lat coordinates of the top left corner

- image tracing: 
    - http://potrace.sourceforge.net/#dual
    - An Algorithm for Automatically Fitting Digitized Curves
    - http://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/alg.html
    - https://en.wikipedia.org/wiki/Boundary_tracing

- generate some nice samples for the documentation
    - Mount Fuji: 138.637436,35.288563,138.822693,35.434156
    - Kilimanjaro: 36.933563,-3.416918,37.721558,-2.825636
    - Etna
    - add batch script that generates some sample images from these sample DEMs

- how to generate code docs?
    - https://fsprojects.github.io/FSharp.Formatting/commandline.html

- investigate possible filters
    - low pass/high pass filters
        - Geospatial Analysis, p. 175
    - use Paint.NET as a start

- hillshading links
    - http://www.saga-gis.org/saga_tool_doc/2.2.0/ta_lighting_0.html
    - https://www.rayshader.com/