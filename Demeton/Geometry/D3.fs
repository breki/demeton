/// <summary>
/// Contains basic 3D geometry primitives and functions.
/// </summary>
module Demeton.Geometry.D3

type Point3 = { X: float; Y: float; Z: float }
type Vector3 = Point3

let triangleNormal a b c: Vector3 = 
    let adx = b.X - a.X
    let ady = b.Y - a.Y
    let adz = b.Z - a.Z
    let bdx = c.X - a.X
    let bdy = c.Y - a.Y
    let bdz = c.Z - a.Z
    
    let ab1 = ady * bdz - adz * bdy
    let ab2 = adz * bdx - adx * bdz
    let ab3 = adx * bdy - ady * bdx

    let len = sqrt(ab1 * ab1 + ab2 * ab2 + ab3 * ab3)

    { X = ab1 / len; Y = ab2 / len; Z = ab3 / len }
