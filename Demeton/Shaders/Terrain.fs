﻿module Demeton.Shaders.Terrain

open System

/// <summary>
/// The heights of four corners of the area we need to calculate slope and
/// orientation for.
/// </summary>
/// <remarks>
/// The four corners are specified in the following order:
/// |---|
/// |0|1|
/// |3|2|
/// |---|
/// </remarks>
type HeightsWindow = float option []

/// <summary>
/// Indicates whether one or more of the <see cref="HeightsWindow" /> values
/// is <c>None</c>.
/// </summary>
let someHeightsAreMissing heightsWindow =
    heightsWindow |> Array.exists Option.isNone

/// <summary>
/// Calculates the slope (in radians) for the specified heights window and the
/// horizontal and vertical sizes of the area (in meters).
/// </summary>
/// <remarks>
/// The slope is defined as the angle between an imaginary vertical line and
/// the normal vector of the area. So, a completely flat horizontal surface will
/// have a slope of 0 degrees, while totally vertical cliff would have a slope
/// of 90 degrees (in radians) (although 90 degrees is impossible to achieve
/// in this model). 
/// </remarks>
let calculateSlopeAndOrientation 
    (heightsWindow: HeightsWindow) horizontalSize verticalSize = 
    let triangleNormalWithX20DiffSameAsX10Diff
        height10Diff height20Diff x10Diff y20Diff = 
        let baHeightDiff = height10Diff
        let caHeightDiff = height20Diff
        
        let vx = -baHeightDiff * y20Diff
        let vy = x10Diff * (baHeightDiff - caHeightDiff)
        let vz = x10Diff * y20Diff
    
        (vx, vy, vz)
    
    let triangleNormalWithx20DiffZero
        height10Diff height20Diff x10Diff y20Diff = 
        let baHeightDiff = height10Diff
        let caHeightDiff = height20Diff
        
        let vx = -baHeightDiff * y20Diff
        let vy = -x10Diff * caHeightDiff
        let vz = x10Diff * y20Diff
    
        (vx, vy, vz)
    
    let triangleNormalToSlopeAndOrientation (nx, ny, nz) =
        let normalXYLen = Math.Sqrt(nx * nx + ny * ny)

        let slope = Math.Atan2(normalXYLen, nz)

        // todo: orientation still needs to be implemented properly
        let orientation = 0.

        (slope, orientation)

    match someHeightsAreMissing heightsWindow with
    | true -> None
    | false ->
        let height0 = Option.get heightsWindow.[0]
        let height1 = Option.get heightsWindow.[1]
        let height2 = Option.get heightsWindow.[2]
        let height3 = Option.get heightsWindow.[3]

        let height02Diff = height0 - height2
        let height10Diff = height1 - height0
        let height12Diff = height1 - height2
        let height20Diff = -height02Diff
        let height30Diff = height3 - height0
        let height32Diff = height3 - height2

        // Calculates normals of four triangles that can be constructed from the
        // four points. Note that the normals are not normalized to length 1,
        // since we don't really need this.
        let triangle1Normal = 
            triangleNormalWithX20DiffSameAsX10Diff
                height10Diff height20Diff horizontalSize verticalSize
        let triangle3Normal = 
            triangleNormalWithx20DiffZero
                height10Diff height30Diff horizontalSize verticalSize
        let triangle2Normal = 
            triangleNormalWithX20DiffSameAsX10Diff
                height32Diff height02Diff -horizontalSize -verticalSize
        let triangle4Normal = 
            triangleNormalWithx20DiffZero
                height32Diff height12Diff -horizontalSize -verticalSize

        // Calculates the slope and orientation from all of the 4 normals.
        let (slope1, orientation1) = 
            triangleNormalToSlopeAndOrientation triangle1Normal
        let (slope2, orientation2) = 
            triangleNormalToSlopeAndOrientation triangle2Normal
        let (slope3, orientation3) = 
            triangleNormalToSlopeAndOrientation triangle3Normal
        let (slope4, orientation4) = 
            triangleNormalToSlopeAndOrientation triangle4Normal

        // The final slope and orientation is an average value from the 4
        // pairs.
        let slope = (slope1 + slope2 + slope3 + slope4) / 4.
        let orientation = 
            (orientation1 + orientation2 + orientation3 + orientation4) / 4.

        Some (slope, orientation)