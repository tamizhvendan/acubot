#r "packages/FAKE/tools/FakeLib.dll"
open Fake
#load "Runner.fsx"

open System

let fileName = "MiniSuave.fsx"
let runnerStepEnvVariable = "RUNNER_STEP"

let isInit = ref false
let runnerStep = ref 0
let fileContent = ref ""
let getRunnerStep () = 
  let value = environVarOrDefault runnerStepEnvVariable "1"
  match Int32.TryParse value with
  | true, step -> 
    if step = 0 then 0 
    else 
      if step > Runner.totalSteps then
        failwithf "invalid step. it should be less than or equal to %d" Runner.totalSteps      
      step - 1
  | _ -> 0



let executeRunner () =
  match Runner.execute !runnerStep with
  | true ->
    let nextStep = !runnerStep + 1
    if nextStep < Runner.totalSteps then
      Runner.printStep nextStep
      runnerStep := nextStep
  | _ -> ()
let watch () =
  use watcher = !! fileName |> WatchChanges (fun changes ->
    let fc = changes |> Seq.head
    let content = fc.FullPath |> System.IO.File.ReadAllText
    if content <> !fileContent then
      printfn "compiling..."
      executeRunner ()
      fileContent := content      
  )
  if !isInit then 
    traceFAKE "Welcome to MiniSuave Workshop"
    let currentStep = getRunnerStep()
    Runner.printStep currentStep
    runnerStep := currentStep
    isInit := false
  System.Console.ReadLine() |> ignore
  watcher.Dispose()

let init () =
  isInit := true

Target "Init" (fun _ -> init ())
Target "Watch" (fun _ -> watch ())

"Init"
==> "Watch"

RunTargetOrDefault "Watch"