/// <summary>
/// Contains common code for working with rasters.
/// </summary>
module Raster

open System

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

    /// <summary>
    /// Represents an empty rectangle.
    /// </summary>
    static member Empty = { 
        MinX = Int32.MaxValue
        MinY = Int32.MaxValue
        Width = 0
        Height = 0 
        }

/// <summary>
/// Represents a location of a pixel.
/// </summary>
type Point = (int * int)

/// <summary>
/// Raw image data represented as a byte array.
/// </summary>
type RawImageData = byte[]
