[<RequireQualifiedAccess>]
module Paths

open System.IO

let combine (toAdd: string) (path: string): string = Path.Combine (path, toAdd)

let extension (extension: string) (path: string): string =
    Path.ChangeExtension(path, extension)
