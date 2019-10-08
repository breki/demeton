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
        let diagonalSize = 
            sqrt (Math.Pow(horizontalSize, 2.) + Math.Pow(verticalSize, 2.))
    
        let diagonalSlope height1 height2 =
            let minHeight = min height1 height2
            let maxHeight = max height1 height2
            let diffHeight = maxHeight - minHeight
            let angleRad = Math.Atan2(diffHeight, diagonalSize)
            radToDeg angleRad

        let slope1 = 
            diagonalSlope 
                (Option.get (heightsWindow.[0])) 
                (Option.get (heightsWindow.[3]))
        let slope2 = 
            diagonalSlope 
                (Option.get (heightsWindow.[1])) 
                (Option.get (heightsWindow.[2]))

        Some ((slope1 + slope2) / 2.)
