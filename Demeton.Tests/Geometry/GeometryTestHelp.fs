module Geometry.GeometryTestHelp

open FsCheck
open FsCheck.Xunit

let randomPointGenerator =
    let x = Gen.choose(-1000, 1000) |> Gen.map (fun i -> float i / 10.)
    let y = Gen.choose(-1000, 1000) |> Gen.map (fun i -> float i / 10.)
    Gen.zip x y

type RandomPoint = float * float
type RandomPointGenerator =
    static member RandomPoint() =
        randomPointGenerator |> Arb.fromGen

type RandomPoints = (float * float)[]
type RandomPointsGenerator =
    static member RandomPoints() =
        randomPointGenerator |> Gen.arrayOf |> Arb.fromGen

type RandomPointsPropertyAttribute() = 
    inherit PropertyAttribute
        (Arbitrary = 
            [| typeof<RandomPointGenerator>; typeof<RandomPointsGenerator> |],
        QuietOnSuccess = true)
