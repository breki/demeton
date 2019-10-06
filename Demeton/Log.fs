/// <summary>
/// Provides methods for logging. Currently only supports logging into console
/// out and error streams.
/// </summary>
[<RequireQualifiedAccess>]
module Log

open System.IO

/// <summary>
/// Specifies the severity level of the log entry.
/// </summary>
type Level =
    /// <summary>
    /// The log entry describes an error.
    /// </summary>
    Error = 0
    /// <summary>
    /// The log entry describes a warning.
    /// </summary>
    | Warn = 1
    /// <summary>
    /// The log entry describes an informational event.
    /// </summary>
    | Info = 2
    /// <summary>
    /// The log entry contains a verbose debugging information.
    /// </summary>
    | Debug = 3

let routeToWriter level: TextWriter option =
    match level with
    | Level.Error -> Some System.Console.Error
    | _ -> Some System.Console.Out

/// <summary>
/// Writes a log message to the log, routing it to the appropriate writer
/// based on the level of the message.
/// </summary>
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

/// <summary>
/// Formats and writes a log entry of the specified level using the 
/// <see cref="writeMessage" /> function.
/// </summary>
let log level format = 
    Printf.kprintf (fun msg -> writeMessage level msg) format

let error format = log Level.Error format
let info format = log Level.Info format
let debug format = log Level.Debug format