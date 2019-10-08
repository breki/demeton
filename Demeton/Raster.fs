/// <summary>
/// Contains common code for working with rasters.
/// </summary>
[<RequireQualifiedAccess>]
module Demeton.Raster

/// <summary>
/// Represents a pixel rectangle.
/// </summary>
type Rect = { MinX: int; MinY: int; Width: int; Height: int }
    with
    /// <summary>
    /// Creates a rectangle by specifying its minimum and maximum X and Y 
    /// coordinates.
    /// </summary>
    static member asMinMax minX minY maxX maxY =
        let width = maxX - minX
        let height = maxY - minY
        { MinX = minX; MinY = minY; Width = width; Height = height }

    member this.MaxX = this.MinX + this.Width
    member this.MaxY = this.MinY + this.Height
