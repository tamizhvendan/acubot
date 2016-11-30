#r "packages/FAKE/tools/FakeLib.dll"
open Fake
#load "Fsi.fsx"
open Fsi
open System
#load "Steps.fsx"
open Steps
open System.IO 

// ------------------
let pickIfEmpty list2 list1 =
  match List.isEmpty list1 with
  | true -> list2
  | _ -> list1
let pickRandomMessage list =
  let random = new System.Random()
  let index = random.Next(0, List.length list)   
  List.item index list   
let green = trace
let yellow = traceFAKE
let whitefn = printfn
let white = printf
let red = traceError
// ------------------
let (|>>) x f = 
  printfn "%A" x
  f x

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


let personalise username (message : string) = 
  message.Replace("%s", username)

let inline goToNext step =
  let os = System.Environment.OSVersion.Platform.ToString()
  let promptChar = if os.ToLowerInvariant().Contains("win") then '>' else '$'
  printfn ""
  let rec prompt printNextStep =
    match printNextStep, step with
    | true, 0 -> 
      yellow "« Type next to continue »"
      white "[MiniSuave%cIntro] %c " Path.DirectorySeparatorChar promptChar
    | true, x when x = steps.Length ->
      System.Environment.Exit(0)
    | true, _ ->         
      yellow "« Type next to continue »"
      white "[MiniSuave%cChallenge%c%d] %c " Path.DirectorySeparatorChar Path.DirectorySeparatorChar step promptChar
    | _ -> ()        
    let command : string = Console.ReadLine()
    if String.Equals(command, "next", StringComparison.InvariantCultureIgnoreCase) then ()
    else if command.Trim() = "" then prompt false
    else prompt true
  prompt true
let rec runAsserts fsi xs =
  match xs with
  | [] -> Success ()
  | x :: xs ->
    match runAssert fsi x with
    | Success _ -> runAsserts fsi xs
    | Error msg -> Error msg
let executeStep stepCount username step =
  use fsi = fsi()
  match evalInteraction fsi """ #load "MiniSuave.fsx";;  """ with
  | Success _ -> 
    match evalInteraction fsi """ open MiniSuave;; """ with
    | Success _ -> 
      match runAsserts fsi step.Asserts with
      | Success _ -> 
        printfn ""
        step.Appreciations        
        |> pickIfEmpty genericAppreciationMsgs
        |> pickRandomMessage  
        |> personalise username
        |> green  
        goToNext stepCount; true
      | Error msg -> printfn ""; red msg; false
    | Error msg -> printfn ""; red msg; false
  | Error msg -> printfn ""; red msg; false
  
let execute stepCount username =
  steps |> List.item stepCount |> executeStep (stepCount + 1) username

let printStep currentStep =
  Console.Clear()
  let step = steps |> List.item currentStep
  green <| sprintf "[Challenge %d of %d] %s" (currentStep+1) steps.Length step.Objective
  printfn ""
  printfn "[QuickHint] %s" step.QuickHint

let totalSteps = steps.Length