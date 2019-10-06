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

/// <summary>
/// Returns the appropriate <see cref="TextWriter" /> to write the log entry to
/// based on the specified severity level of the log entry. For error level,
/// the function uses <see cref="System.Console.Error" /> writer, while for all
/// the others it uses <see cref="System.Console.Out" />.
/// </summary>
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

/// <summary>
/// Writes a log entry with the <see cref="Level.Error" /> severity level.
/// </summary>
let error format = log Level.Error format

/// <summary>
/// Writes a log entry with the <see cref="Level.Info" /> severity level.
/// </summary>
let info format = log Level.Info format

/// <summary>
/// Writes a log entry with the <see cref="Level.Debug" /> severity level.
/// </summary>
let debug format = log Level.Debug format