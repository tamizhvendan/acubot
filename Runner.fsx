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

let runAssert fsi = function
| Compiler content -> evalInteraction fsi content
| Compiler2 (content, errorMsg) ->
  match evalInteraction fsi content with
  | Success v -> Success v
  | Error _ -> Error errorMsg
| Expression (content, expected) -> 
  match evalExpression fsi content with
  | Success value -> 
    match value with
    | Some v -> 
      let actual = v.ReflectionValue |> sprintf "%A" 
      if actual = expected then
       Success () 
      else
        sprintf "Expected %s but found %s" expected actual |> Error
    | None -> Success ()
  | Error msg -> Error msg

let rec runAsserts fsi xs =
  match xs with
  | [] -> Success ()
  | x :: xs ->
    match runAssert fsi x with
    | Success _ -> runAsserts fsi xs
    | Error msg -> Error msg
let executeStep step =
  use fsi = fsi()
  match evalInteraction fsi """ #load "MiniSuave.fsx";;  """ with
  | Success _ -> 
    match evalInteraction fsi """ open MiniSuave;; """ with
    | Success _ -> 
      match runAsserts fsi step.Asserts with
      | Success _ -> printfn "%s" step.Greeting
      | Error msg -> printfn "%s" msg
    | Error msg -> printfn "%s" msg
  | Error msg -> printfn "%s" msg
  

steps |> List.item 2 |> executeStep