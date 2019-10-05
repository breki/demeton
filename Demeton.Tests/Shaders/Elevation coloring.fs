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
    test <@ scale |> colorOfHeight (Some 700.) = Some color700 @>
    test <@ scale |> colorOfHeight (Some 1000.) = Some color1000 @>

[<Fact>]
let ``If height is below the minimal scale mark, use the color of the that minimal scale mark``() =
    test <@ scale |> colorOfHeight (Some 500.) = Some color700 @>

[<Fact>]
let ``If height is above the maximal scale mark, use the color of the that maximal scale mark``() =
    test <@ scale |> colorOfHeight (Some 1200.) = Some color1000 @>
    
[<Fact>]
let ``If height is None and scale has a color configured for it, return it``() =
    let colorNone = Rgba8Bit.rgbaColor 10uy 10uy 10uy 10uy
    
    let scale = { 
        Marks = [| (DemHeight 700, color700); (DemHeight 1000, color1000) |]; 
        NoneColor = Some colorNone 
        }
    
    test <@ scale |> colorOfHeight None = Some colorNone @>
    
[<Fact>]
let ``If height is None and scale has does not have a color configured for it, return None``() =
    let scale = { 
        Marks = [| (DemHeight 700, color700); (DemHeight 1000, color1000) |]; 
        NoneColor = None
        }
    
    test <@ scale |> colorOfHeight None = None @>

[<Fact>]
let ``If height is inbetween two scale marks, interpolate between their colors``() =
    test <@ scale |> colorOfHeight (Some 850.) = 
        Some (Rgba8Bit.rgbaColor 150uy 150uy 150uy 150uy) @>
