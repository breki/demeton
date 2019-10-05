module Tests.Png.``Mixing Rgba8Bit colors``

open Xunit
open Swensen.Unquote

open Png

let colorA = Rgba8Bit.rgbaColor 10uy 20uy 30uy 40uy
let colorB = Rgba8Bit.rgbaColor 110uy 120uy 130uy 140uy

[<Fact>]
let ``If mixing ratio is 0 return the first color exactly``() =
    test <@ Rgba8Bit.mixColors colorA colorB 0. = colorA @>

[<Fact>]
let ``If mixing ratio is 1 return the second color exactly``() =
    test <@ Rgba8Bit.mixColors colorA colorB 1. = colorB @>

[<Fact>]
let ``If mixing ratio is between 0 and 1 return the mixed color``() =
    test <@ Rgba8Bit.mixColors colorA colorB 0.5 = 
        Rgba8Bit.rgbaColor 60uy 70uy 80uy 90uy @>
