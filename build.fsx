#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.Testing.XUnit2
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators


// Properties
let buildDir = "./build/"
let testDir  = "./test/"

// Targets
Target.create "Clean" (fun _ ->
    Shell.cleanDirs [buildDir; testDir]
)

Target.create "Release" (fun _ ->
   !! "Demeton.sln"
   |> MSBuild.runRelease id buildDir "Build"
   |> Trace.logItems "AppBuild-Output: "
)

Target.create "Test" (fun _ -> 
    DotNet.test (fun options -> options) "Demeton.Tests"
)

open Fake.Core.TargetOperators
"Clean"
    ==> "Release"
        ==> "Test"

// start build
Target.runOrDefault "Test"