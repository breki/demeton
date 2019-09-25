/// <summary>
/// Contains functions for manipulating file paths in a functional way.
/// </summary>
[<RequireQualifiedAccess>]
module Pth

open System.IO

/// <summary>
/// Combines two strings into a path.
/// </summary>
let combine (toAdd: string) (path: string): string = Path.Combine (path, toAdd)

/// <summary>
/// Changes the extension of a path string.
/// </summary>
let extension (extension: string) (path: string): string =
    Path.ChangeExtension(path, extension)

/// <summary>
/// Returns the directory information for the specified path string.
/// </summary>
let directory (path: string): string = Path.GetDirectoryName(path)

/// <summary>
/// Returns the file name of the specified path string without the extension.
/// </summary>
let fileNameWithoutExtension (path: string) = 
    Path.GetFileNameWithoutExtension(path)
