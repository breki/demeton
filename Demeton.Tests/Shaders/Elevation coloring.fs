module Tests.Shaders.``Elevation coloring``

open Demeton.Dem.Types
open Demeton.Shaders
open Png

open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit
open PropertiesHelp

let colorNone = Rgba8Bit.rgbaColor 0uy 0uy 0uy 0uy
let color700 = Rgba8Bit.rgbaColor 100uy 100uy 100uy 100uy
let color1000 = Rgba8Bit.rgbaColor 200uy 200uy 200uy 200uy

let scale: ElevationColoring.ColorScale =
    { Marks = [| (DemHeight 700, color700); (DemHeight 1000, color1000) |]
      NoneColor = colorNone }

[<Fact>]
let ``If height falls exactly on one of the marks, use its color directly`` () =
    test <@ scale |> ElevationColoring.colorOfHeight (Some 700.) = color700 @>
    test <@ scale |> ElevationColoring.colorOfHeight (Some 1000.) = color1000 @>

[<Fact>]
let ``If height is below the minimal scale mark, use the color of the that minimal scale mark``
    ()
    =
    test <@ scale |> ElevationColoring.colorOfHeight (Some 500.) = color700 @>

[<Fact>]
let ``If height is above the maximal scale mark, use the color of the that maximal scale mark``
    ()
    =
    test <@ scale |> ElevationColoring.colorOfHeight (Some 1200.) = color1000 @>

[<Fact>]
let ``If height is None, return the color the scale has configured for it`` () =
    let scale: ElevationColoring.ColorScale =
        { Marks = [| (DemHeight 700, color700); (DemHeight 1000, color1000) |]
          NoneColor = colorNone }

    test <@ scale |> ElevationColoring.colorOfHeight None = colorNone @>

[<Fact>]
let ``If height is inbetween two scale marks, interpolate between their colors``
    ()
    =
    test
        <@
            scale |> ElevationColoring.colorOfHeight (Some 850.) = Rgba8Bit.rgbaColor
                150uy
                150uy
                150uy
                150uy
        @>

[<Property(Verbose = false)>]
let ``Elevation coloring properties`` () =
    let returnsColorNoneForHeightNone heightMaybe =
        Option.isNone heightMaybe
        ==> let colorReturned =
                scale |> ElevationColoring.colorOfHeight heightMaybe in

            colorReturned = colorNone
            |> Prop.classify (Option.isNone heightMaybe) "None height"
            |> Prop.collect None

    let returnsMaxColorWhenHeightIsAboveMaxMark heightMaybe =
        match heightMaybe with
        | (Some height) when height > 1000. -> true
        | _ -> false
        ==> let colorReturned =
                scale |> ElevationColoring.colorOfHeight heightMaybe in

            colorReturned = color1000
            |> Prop.classify (true) "Height above max mark"
            |> Prop.collect "Above"

    let returnsMinColorWhenHeightIsBelowMinMark heightMaybe =
        match heightMaybe with
        | (Some height) when height < 700. -> true
        | _ -> false
        ==> let colorReturned =
                scale |> ElevationColoring.colorOfHeight heightMaybe in

            colorReturned = color700
            |> Prop.classify (true) "Height below min mark"
            |> Prop.collect "Below"

    let returnsExactMarkColorIfHeightIsExactlyOnMark heightMaybe =
        match heightMaybe with
        | (Some height) when height = 700. || height = 1000. -> true
        | _ -> false
        ==> lazy
            let colorReturned =
                scale |> ElevationColoring.colorOfHeight heightMaybe

            let height = Option.get heightMaybe

            ((height = 700. && colorReturned = color700)
             || (height = 1000. && colorReturned = color1000))
            |> Prop.classify (true) "Height exact on mark"
            |> Prop.collect "Exact"

    let allProperties x =
        (returnsColorNoneForHeightNone x)
        .|. (returnsMinColorWhenHeightIsBelowMinMark x)
        .|. (returnsMaxColorWhenHeightIsAboveMaxMark x)
        .|. (returnsExactMarkColorIfHeightIsExactlyOnMark x)

    let genHeightSome = floatInRangeInclusive -1000 2000 |> Gen.map Some
    let genHeightExactOnMark = Gen.constant 700. |> Gen.map Some

    let genHeightMaybe =
        Gen.frequency[8, genHeightSome
                      1, genHeightExactOnMark
                      1, gen { return None }]

    genHeightMaybe |> Arb.fromGen |> Prop.forAll <| allProperties
