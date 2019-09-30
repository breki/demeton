module Projections.ProjectionsTestHelpers

open TestHelp
open FsCheck
open FsCheck.Xunit

let expectXY x y point =
    match point with
    | None -> 
        raise (Xunit.Sdk.XunitException("Expected some point, but got None."))
    | Some (actualX, actualY) -> 
        actualX |> isApproxEqualTo x 8
        actualY |> isApproxEqualTo y 8

let expectNone point =
    match point with
    | Some _ -> 
        raise (Xunit.Sdk.XunitException("Expected no point, but got some."))
    | None -> ()

type ProjectLonLat = float * float
type ProjectLonLatGenerator =
    static member LonLat() =
        let x = Gen.choose (-1000, 1000)
        let y = Gen.choose (-1000, 1000)
        Gen.zip x y 
        |> Gen.map (
            fun (x, y) -> (float x / 1000. * 180., float y / 1000. * 90.))
        |> Arb.fromGen

type ProjectLonLatAttribute() =
    inherit PropertyAttribute
        (Arbitrary = [| typeof<ProjectLonLatGenerator> |],
        QuietOnSuccess = true)

