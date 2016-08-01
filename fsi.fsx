#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"

open FSharp.Data

[<Literal>]
let OutputJson = """
  {
    "result": "output",
    "output": "\r\nF# Interactive for F# 4.0 (private)\r\nFreely dist...",
    "details": null
  }
"""
type FsiOutput = JsonProvider<OutputJson>

let output host =
  Http.RequestString (host + "/output", httpMethod = "POST")
  |> FsiOutput.Parse


let (|Success|Error|Exception|Unknown|) = function
| "success" -> Success
| "error" -> Error
| "exception" -> Exception
| _ -> Unknown

type EvalSuccess = {
  Output : string
  Details : string
}

type EvalError = {
  FileName : string
  Start : int
  End : int
  Line : int
  Message : string
}

type EvalException = {
  Details : string
}

type Result =
| EvalSuccess of EvalSuccess
| EvalErrors of EvalError list
| EvalException of EvalException
| Unknown


[<Literal>]
let EvalJson = """[{
    "result": "success",
    "output": "val it : int = 2\r\n",
    "details": {
      "string": "test",
      "html": null,
      "warnings": []
    }
  },{
    "result": "error",
    "output": "Stopped due to error\n",
    "details": [
      {
        "startLine": 10,
        "endLine": 10,
        "startColumn": 2,
        "endColumn": 5,
        "fileName": "/a.fsx",
        "severity": "error",
        "errorNumber": 1,
        "message": "The type 'float' does not match the type 'int'"
      }]
  }, {
    "result": "exception",
    "output": "",
    "details": "System.Exception: Oops!\r\n   at <StartupCode$FSI_00..."
  }]
"""

type FsiEval = JsonProvider<EvalJson, SampleIsList=true>

let toResult (fsiEval : FsiEval.Root) =
  match fsiEval.Result with
  | Success ->
    let output =
      match fsiEval.Output with
      | Some output -> output
      | _ -> ""
    let details =
      match fsiEval.Details.Record with
      | Some d -> d.String
      | _ -> ""
    EvalSuccess {Output = output; Details = details}
  | Error ->
    match fsiEval.Details.Array with
    | Some errors ->
      errors
      |> Array.map (fun e -> {
                              FileName = e.FileName
                              Start = e.StartColumn
                              End = e.EndColumn
                              Line = e.StartLine
                              Message = e.Message
                            })
      |> Array.toList |> EvalErrors
    | _ -> EvalErrors []
  | Exception ->
    match fsiEval.Details.String with
    | Some v -> { Details = v} |> EvalException
    | None -> {Details = ""} |> EvalException
  | _ -> Unknown

let eval fileName host (content : string) =
  let body =
    content.Replace("\"", "\\\"")
    |> sprintf """{ "file":"/%s", "line":0, "code":"%s"}""" fileName
    |> TextRequest
  Http.RequestString(host + "/eval", httpMethod = "POST", body = body)
  |> FsiEval.Parse
  |> toResult
