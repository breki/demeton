﻿- how to speed things up?
    - use tuple instead of GlobalCellCoords?
- initializing the random array is very slow
    - probably because we are yielding a subsequence
    - use mutable array instead?
- implement reading of the actual HGT file
    - function that reads data from a stream, but expects 3601*3601 entries
    - function that creates HeightsArray from these entries
