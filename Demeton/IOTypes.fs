module Demeton.IOTypes

open System
open System.IO;

type FunctionalStreamReader(stream: Stream) =
    let mutable lastByteRead: int option = None

    member this.moveForward() =
        lastByteRead <- Some (stream.ReadByte())
        lastByteRead <> Some -1

    member this.currentByte(): byte =
        match lastByteRead with
        | None -> raise
                    (InvalidOperationException
                        ("Please call moveForward first."))
        | Some -1 -> raise
                        (InvalidOperationException("End of stream reached."))
        | Some x -> (byte)x
       
    member this.stream = stream
