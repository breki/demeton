[<RequireQualifiedAccess>]
module Log

open System.IO

type Level =
    Error = 0
    | Warn = 1
    | Info = 2
    | Debug = 3

let routeToWriter level: TextWriter option =
    match level with
    | Level.Error -> Some System.Console.Error
    | _ -> Some System.Console.Out

let writeMessage level (message: string) = 
    let writer = routeToWriter level
    match writer with
    | Some wr -> 
        let timestamp = 
            System.DateTime.Now.ToString
                (
                "u",
                System.Globalization.CultureInfo.InvariantCulture)
        wr.WriteLine(timestamp + " | " + message)
        wr.Flush()
        ignore()
    | None -> ()

let log level format = 
    Printf.kprintf (fun msg -> writeMessage level msg) format

let error format = log Level.Error format
let info format = log Level.Info format