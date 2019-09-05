module Demeton.IOTypes

open System
open System.IO;

type FunctionalStreamReader(stream: Stream) =
    let mutable lastByteRead: int = -1

    member this.moveForward() =
        lastByteRead <- stream.ReadByte()
        lastByteRead <> -1

    member this.currentByte(): byte =
        match lastByteRead with
        | -1 -> invalidOp "End of stream reached."
        | x -> (byte)x
       
    member this.stream = stream
