module Tests.``Acceptance tests``.``Generating hillshading rasters``

open System
open System.Diagnostics

open Xunit
open Swensen.Unquote

let consoleDir = Environment.GetEnvironmentVariable("DEMETON_CONSOLE_DIR")
let cacheDir = Environment.GetEnvironmentVariable("SRTM_CACHE")

// todo: finish the acceptance test
[<Fact>]
[<Trait("Category", "acceptance")>]
let ``Generate hillshade raster that requires SRTM level 1``() =
    test <@ consoleDir <> null @>
    test <@ cacheDir <> null @>

    let args = 
        sprintf
            "shade 13.476104,46.330401,14.020477,46.690581 --map-scale 1000000 --dpi 1 --local-cache-dir %s"
            cacheDir

    let cmd = new Process()
    cmd.StartInfo.FileName <-
        consoleDir |> Pth.combine "Demeton.Console.exe"
    cmd.StartInfo.Arguments <- args
    cmd.StartInfo.RedirectStandardInput <- true
    cmd.StartInfo.RedirectStandardOutput <- true
    cmd.StartInfo.RedirectStandardError <- true
    cmd.StartInfo.CreateNoWindow <- true
    cmd.StartInfo.UseShellExecute <-false
    test <@ cmd.Start() @>

    cmd.StandardInput.Flush()
    cmd.StandardInput.Close()
    cmd.WaitForExit()
    test <@ cmd.ExitCode = 0 @>

    Console.WriteLine(cmd.StandardOutput.ReadToEnd())
    Console.WriteLine(cmd.StandardError.ReadToEnd())

    //let coloringParameters 
    //    = { ColorScale = ElevationColoring.colorScaleMaperitive }
    
    //let options = {
    //    CoveragePoints = [ (13.476104,46.330401); (14.020477,46.690581) ]
    //    FilePrefix = ShadeCommand.DefaultFilePrefix
    //    LocalCacheDir = cacheDir
    //    OutputDir = "."
    //    SrtmDir = "srtm"
    //    TileSize = 1000
    //    RootShadingStep = Pipeline.Common.ElevationColoring coloringParameters
    //    MapScale = { MapScale = 10000; Dpi = 1 }
    //    }

    //let generateTile() = 
    //    generateShadedRasterTile
    //        fetchHeightsArray

    //let results = ShadeCommand.run options generateTile saveTile


