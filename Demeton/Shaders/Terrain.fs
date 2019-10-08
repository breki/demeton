module Demeton.Shaders.Terrain

open Demeton.Geometry.Common

open System

type HeightsWindow = float option []

let someHeightsAreMissing heightsWindow =
    heightsWindow |> Array.exists Option.isNone

let slope (heightsWindow: HeightsWindow) horizontalSize verticalSize = 
    match someHeightsAreMissing heightsWindow with
    | true -> None
    | false ->
        let edgeSlope height1 height2 edgeSize =
            let minHeight = min height1 height2
            let maxHeight = max height1 height2
            let diffHeight = maxHeight - minHeight
            let angleRad = Math.Atan2(diffHeight, edgeSize)
            radToDeg angleRad

        let slope01 = 
            edgeSlope 
                (Option.get (heightsWindow.[0])) 
                (Option.get (heightsWindow.[1]))
                horizontalSize
        let slope23 = 
            edgeSlope
                (Option.get (heightsWindow.[2])) 
                (Option.get (heightsWindow.[3]))
                horizontalSize

        let horizontalAvgSlope = (slope01 + slope23) / 2.

        let slope02 = 
            edgeSlope 
                (Option.get (heightsWindow.[0])) 
                (Option.get (heightsWindow.[2]))
                verticalSize
        let slope13 = 
            edgeSlope
                (Option.get (heightsWindow.[1])) 
                (Option.get (heightsWindow.[3]))
                verticalSize

        let verticalAvgSlope = (slope02 + slope13) / 2.

        Some (max horizontalAvgSlope verticalAvgSlope)
