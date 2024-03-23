module Tests.Rasters.Rect_tests

open Raster
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Extend empty Rect by a point`` () =
    let rect = Rect.Empty
    let rect' = rect.Extend(10, 20)

    test
        <@
            rect' = { MinX = 10
                      MinY = 20
                      Width = 1
                      Height = 1 }
        @>

[<Fact>]
let ``Extend Rect by another point by just resizing it`` () =
    let rect = Rect.Empty
    let rect' = rect.Extend(10, 20)
    let rect'' = rect'.Extend(11, 21)

    test
        <@
            rect'' = { MinX = 10
                       MinY = 20
                       Width = 2
                       Height = 2 }
        @>

[<Fact>]
let ``Extend Rect by another point by moving anchor point`` () =
    let rect = Rect.Empty
    let rect' = rect.Extend(11, 21)
    let rect'' = rect'.Extend(10, 20)

    test
        <@
            rect'' = { MinX = 10
                       MinY = 20
                       Width = 2
                       Height = 2 }
        @>
