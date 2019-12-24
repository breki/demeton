/// Provides a mechanism for logging into a file for test debugging purposes.
module TestLog

open Serilog
open Serilog.Events
open System.IO

/// Initializes logging to a file.
let initLog logFileName =
    let outputTemplate =
        "{Timestamp:yy-MM-dd HH:mm:ss.fff} [{Level:u3}] {Message:lj}{NewLine}{Exception}"
    
    if File.Exists logFileName then File.Delete logFileName
        
    LoggerConfiguration().WriteTo
        .File(logFileName, LevelAlias.Minimum, outputTemplate)
        .CreateLogger()

