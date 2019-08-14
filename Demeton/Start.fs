module Demeton

type Bounds = { 
    MinLon: double
    MinLat: double 
    MaxLon: double 
    MaxLat: double
    }

[<StructuredFormatDisplay("SrtmTile ({Lon}, {Lat})")>]
type SrtmTile = {
    Lon: int
    Lat: int
    }

let boundsToTiles (bounds: Bounds): SrtmTile list =
    let allLons = 
        [ floor bounds.MinLon |> int 
            .. (ceil bounds.MaxLon |> int) - 1]
    let allLats = 
        [ floor bounds.MinLat |> int 
            .. (ceil bounds.MaxLat |> int) - 1]

    [ 
        for lat in allLats do
            for lon in allLons do
                yield { Lon = lon; Lat = lat; } 
    ]

