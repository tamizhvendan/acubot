#r "packages/FAKE/tools/FakeLib.dll"
#load "fsi.fsx"
open Fsi
open Fake
open System.Diagnostics

Target "FsiInteractive" (fun _ ->
  StartProcess (fun info ->
    info.FileName <- "packages/FsInteractiveService.Http/tools/FsInteractiveService.exe"
    info.Arguments <- "18082"
  )
)

let fileName = "runner.fsx"
let fsiHost = "http://localhost:18082"

let eval () =
  fileName
  |> System.IO.File.ReadAllText
  |> eval fileName fsiHost
  |> printfn "%A"

Target "Watch" (fun _ ->
  use watcher = !! fileName |> WatchChanges (fun changes ->
      tracefn "compiling..."
      eval()
  )
  System.Console.ReadLine() |> ignore
  watcher.Dispose()
  killAllCreatedProcesses()
)

"FsiInteractive"
==> "Watch"

RunTargetOrDefault "Watch"