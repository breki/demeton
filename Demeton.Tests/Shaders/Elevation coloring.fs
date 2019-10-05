module Shaders.``Elevation coloring``

open Demeton.DemTypes
open Demeton.Shaders.ElevationColoring
open Png

open Xunit
open Swensen.Unquote

let color700 = Rgba8Bit.rgbaColor 100uy 100uy 100uy 100uy
let color1000 = Rgba8Bit.rgbaColor 200uy 200uy 200uy 200uy

let scale = { 
    Marks = [| (DemHeight 700, color700); (DemHeight 1000, color1000) |]; 
    NoneColor = None 
    }

[<Fact>]
let ``If height falls exactly on one of the marks, use its color directly``() =
    test <@ scale |> colorOfHeight 700s = Some color700 @>
    test <@ scale |> colorOfHeight 1000s = Some color1000 @>

[<Fact>]
let ``If height is below the minimal scale mark, use the color of the that minimal scale mark``() =
    test <@ scale |> colorOfHeight 500s = Some color700 @>

[<Fact>]
let ``If height is above the maximal scale mark, use the color of the that maximal scale mark``() =
    test <@ scale |> colorOfHeight 1200s = Some color1000 @>
    
[<Fact>]
let ``If height is None and scale has a color configured for it, return it``() =
    let colorNone = Rgba8Bit.rgbaColor 10uy 10uy 10uy 10uy
    
    let scale = { 
        Marks = [| (DemHeight 700, color700); (DemHeight 1000, color1000) |]; 
        NoneColor = Some colorNone 
        }
    
    test <@ scale |> colorOfHeight DemHeightNone = Some colorNone @>
    
[<Fact>]
let ``If height is None and scale has does not have a color configured for it, return None``() =
    let scale = { 
        Marks = [| (DemHeight 700, color700); (DemHeight 1000, color1000) |]; 
        NoneColor = None
        }
    
    test <@ scale |> colorOfHeight DemHeightNone = None @>
