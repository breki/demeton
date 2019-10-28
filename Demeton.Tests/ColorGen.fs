[<RequireQualifiedAccess>]
module ColorGen

open Png
open FsCheck

let color = Arb.generate<Rgba8Bit.RgbaColor>

let private genByte = Arb.generate<byte>

let colorWith1Alpha = 
    genByte |> Gen.arrayOfLength 3
    |> Gen.map (fun rgb -> 
        Rgba8Bit.rgbaColor rgb.[0] rgb.[1] rgb.[2] 255uy)

let colorWith0Alpha = 
    genByte |> Gen.arrayOfLength 3
    |> Gen.map (fun components -> 
        Rgba8Bit.rgbaColor components.[0] components.[1] components.[2] 0uy)
