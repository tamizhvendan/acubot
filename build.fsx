#r "packages/FAKE/tools/FakeLib.dll"
open Fake
#load "Runner.fsx"
#load "Common.fsx"

open System
open Common

let fileName = "MiniSuave.fsx"
let runnerStepEnvVariable = "RUNNER_STEP"

let isInit = ref false

let username = ref "buddy"
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

let msgsForCompilation = [
  "Hmm. Let's see..."
  "Hold on when I crunch on some bytes"
  "Will be back with you in a jiffy. My circuits are processing our code"
  "I am feeling lucky..."
]
let prompt step  =
  Console.ForegroundColor <- ConsoleColor.White
  white "%s λ " step
  Console.ReadLine()

let introMsg () =
  Console.Clear()
  green "Hi there! I am Mr.AcuBot, your fellow agent in this  Mission: MiniSuave" 
  green "May I know your name?"
  let name = prompt "[Intro]"
  if name.Trim() = "" then
    yellow "Wanna remain anonymous? No worries. Let me call you 'buddy'!"
  else
    username := name
  green <| sprintf "Hello %s, Pleasure meeting you! Let's get started on Mission: MiniSuave." !username
  Runner.goToNext 0
  ()

let executeRunner () =
  match Runner.execute !runnerStep !username with
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
      printfn ""
      yellow  <| "%s" <| pickRandomMessage msgsForCompilation
      executeRunner ()
      fileContent := content      
  )
  if !isInit then 
    introMsg()
    let currentStep = getRunnerStep()
    Runner.printStep currentStep
    runnerStep := currentStep
    isInit := false

  System.Threading.Thread.Sleep(3 * 60 * 60 * 1000)
  watcher.Dispose()

let init () =
  isInit := true

Target "Init" (fun _ -> init ())
Target "Watch" (fun _ -> watch ())

"Init"
==> "Watch"

RunTargetOrDefault "Watch"