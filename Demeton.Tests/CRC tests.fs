module Demeton.Tests.``CRC tests``

open Demeton.CRC

open FsCheck
open FsCheck.Xunit
open FsUnit
open Xunit
open Swensen.Unquote

[<Property>]
let ``Both CRC implementations calculate the same value``(data: byte[]) =
    let crc1 = crc32 data
    let crc2 = crc32_2 data
    let crc3 = crc32_3 data
    let crc4 = crc32_4 data
    let crc5 = crc32_5 data
    let crc6 = crc32_6 data
    crc1 = crc2 && crc1 = crc3 && crc1 = crc4 && crc1 = crc5 && crc1 = crc6
    
[<Fact>]
let ``CRC performance test``() =
    let rnd = new System.Random(34545)
    let data: byte[] = 
        Array.init 3000000 (fun _ -> byte (rnd.Next(256)))

    let crc1 = crc32 data
    let crc2 = crc32_2 data
    let crc3 = crc32_3 data
    let crc4 = crc32_4 data
    let crc5 = crc32_5 data
    let crc6 = crc32_6 data
    
    test <@ crc1 = crc2 @>
    test <@ crc1 = crc3 @>
    test <@ crc1 = crc4 @>
    test <@ crc1 = crc5 @>
    test <@ crc1 = crc6 @>

[<Fact>]
let ``Reverse bytes``() =
    let reversed: uint32[] = Array.init 256 (fun i -> bitrev8 (byte i) <<< 24)
    
    printfn 
        "reversed: %s" 
        (reversed |> Array.map (fun i -> i.ToString()) |> String.concat "u; ")
        

    

