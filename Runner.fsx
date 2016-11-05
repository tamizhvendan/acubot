#r "packages/FAKE/tools/FakeLib.dll"
open Fake
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


let assertExpression fsi content expected =
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

let runAssert fsi = function
| Compiler content -> evalInteraction fsi content
| Compiler2 (content, errorMsg) ->
  match evalInteraction fsi content with
  | Success v -> Success v
  | Error _ -> Error errorMsg
| Expression (content, expected) -> 
  assertExpression fsi content expected  
| Expression2 (content, expected, errMsg) ->
  match assertExpression fsi content expected with
  | Success _ -> Success ()
  | Error msg -> Error (sprintf "[Assertion (%s) failed] : %s" errMsg msg)

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
      | Success _ -> tracefn "%s" step.Greeting; true
      | Error msg -> traceError msg; false
    | Error msg -> traceError msg; false
  | Error msg -> traceError msg; false
  
let execute stepCount =
  steps |> List.item stepCount |> executeStep

let printStep currentStep =
  let step = steps |> List.item currentStep
  tracefn "[%d of %d] %s" (currentStep+1) steps.Length step.Description

let totalSteps = steps.Length