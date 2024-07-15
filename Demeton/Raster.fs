/// <summary>
/// Contains common code for working with rasters.
/// </summary>
module Raster

open System


/// <summary>
/// Represents a location of a pixel.
/// </summary>
type Point = int * int

/// <summary>
/// Represents a pixel rectangle.
/// </summary>
type Rect =
    { MinX: int
      MinY: int
      Width: int
      Height: int }

    /// <summary>
    /// Creates a rectangle by specifying its minimum and maximum X and Y
    /// coordinates.
    /// </summary>
    static member asMinMax minX minY maxX maxY =
        let width = maxX - minX
        let height = maxY - minY

        { MinX = minX
          MinY = minY
          Width = width
          Height = height }

    member this.MaxX = this.MinX + this.Width
    member this.MaxY = this.MinY + this.Height

    member this.Extend((x, y): Point) =
        let minX, width =
            if this.Width > 0 then
                if x < this.MinX then x, this.MaxX - x
                elif x >= this.MaxX then this.MinX, x - this.MinX + 1
                else this.MinX, this.Width
            else
                x, 1

        let minY, height =
            if this.Height > 0 then
                if y < this.MinY then y, this.MaxY - y
                elif y >= this.MaxY then this.MinY, y - this.MinY + 1
                else this.MinY, this.Height
            else
                y, 1

        { MinX = minX
          MinY = minY
          Width = width
          Height = height }

    /// <summary>
    /// Represents an empty rectangle.
    /// </summary>
    static member Empty =
        { MinX = Int32.MaxValue
          MinY = Int32.MaxValue
          Width = 0
          Height = 0 }

/// <summary>
/// Raw image data represented as a byte array.
/// </summary>
type RawImageData = byte[]

/// <summary>
/// Returns a rectangle that was inflated by the specified size (in pixels)
/// on each side.
/// </summary>
let inflate size rect =
    let minX = rect.MinX - size
    let minY = rect.MinY - size
    let width = rect.Width + 2 * size
    let height = rect.Height + 2 * size

    { MinX = minX
      MinY = minY
      Width = width
      Height = height }


/// <summary>
/// Determines if two rectangles intersect.
/// </summary>
/// <param name="rect1">The first rectangle to check for intersection.</param>
/// <param name="rect2">The second rectangle to check for intersection.</param>
/// <returns>True if the rectangles intersect, otherwise false.</returns>
let doRectsIntersect (rect1: Rect) (rect2: Rect) =
    let xIntersect = rect1.MinX <= rect2.MaxX && rect1.MaxX >= rect2.MinX
    let yIntersect = rect1.MinY <= rect2.MaxY && rect1.MaxY >= rect2.MinY
    xIntersect && yIntersect
