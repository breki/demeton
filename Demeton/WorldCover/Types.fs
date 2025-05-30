﻿module Demeton.WorldCover.Types


[<Literal>]
let WorldCoverYear = 2021

[<Literal>]
let WorldCoverVersion = "v200" // year 2021

[<Literal>]
let WorldCoverS3Domain = "https://esa-worldcover.s3.eu-central-1.amazonaws.com"

let geoJsonUrl = $"{WorldCoverS3Domain}/esa_worldcover_grid.geojson"

[<Literal>]
let WorldCoverDirName = "WorldCover"

[<Literal>]
let WorldCoverTileSize = 12000

[<Literal>]
let WorldCoverTilesInBatch = 3

[<Literal>]
let WorldCoverDegreesPerFile = WorldCoverTilesInBatch

[<Literal>]
let WorldCoverCellsPerDegree = WorldCoverTileSize

[<Literal>]
let WorldCoverBitmapSize = WorldCoverTileSize * WorldCoverTilesInBatch


type WorldCoverTileName = string
