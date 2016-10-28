#load "MiniSuave.fsx"
open MiniSuave

let run webpart =
  let req = {HttpMethod = Get; Path = "/"; Headers = []}
  let res = {StatusCode = NotFound; Content = ""; Headers = []}
  let ctx = {Request = req; Response = res}
  let result = webpart ctx |> Async.RunSynchronously
  match result with
  | Some ctx ->
    printfn "[Status Code - %A]:%A" ctx.Response.StatusCode ctx.Response.Content 
  | None ->
    printfn "No WebPart found"
