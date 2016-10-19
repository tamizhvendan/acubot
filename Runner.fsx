//#r "packages/FAKE/tools/FakeLib.dll"
//open Fake
#load "Fsi.fsx"
open Fsi

#load "Steps.fsx"
open Steps



// Get Current Step From Env Variable
// Load Current Assert
// Open Current Assert
// Get Assert Info
// Print the assert and Wait for the file change
// If no change in MiniSuave keep waiting
// Load MiniSuave
// Open MiniSuave
// Execute the Assert 
//  True - 
//      Update the next step
//      Recurse to start
//  False -
//      Show the error
//      Recurse to file change



//let runnerStep = environVarOrDefault "RUNNER_STEP" "1" |> int

let runAssert = function
| Compiler content -> evalInteraction content

let rec runAsserts xs =
  match xs with
  | [] -> Success ()
  | x :: xs ->
    match runAssert x with
    | Success _ -> runAsserts xs
    | Error msg -> Error msg
let executeStep step =
  match evalInteraction """ #load "MiniSuave.fsx";;  """ with
  | Success _ -> 
    match evalInteraction """ open MiniSuave;; """ with
    | Success _ -> 
      match runAsserts step.Asserts with
      | Success _ -> printfn "%s" step.Greeting
      | Error msg -> printfn "%s" msg
    | Error msg -> printfn "%s" msg
  | Error msg -> printfn "%s" msg

steps |> List.item 0 |> executeStep