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

let executeWebPart inputContext webpart =
  async {
    let! outputContext = webpart inputContext
    match outputContext with
    | Some ctx ->
      printfn "[Status Code - %A]:%A" ctx.Response.StatusCode ctx.Response.Content
      if not (List.isEmpty ctx.Response.Headers) then
        printfn "Headers - %A" ctx.Response.Headers
    | None ->
      printfn "No WebPart found"
  } |> Async.RunSynchronously

let parseRequest (input : System.String) =
  let parts = input.Split([|';'|])
  if parts.Length <> 2 then
    None
  else 
    let rawType = parts.[0]
    let route = parts.[1]
    match rawType with
    | "Get" -> Some {HttpMethod = Get; Path = route; Headers = []}
    | "Post" -> Some {HttpMethod = Post; Path = route; Headers = []}
    | "Put" -> Some {HttpMethod = Put; Path = route; Headers = []}
    | "Delete" -> Some {HttpMethod = Delete; Path = route; Headers = []}
    | _ -> None

let execute input webpart =
  match parseRequest input with
  | Some req -> 
    let res = {StatusCode = Ok; Content = ""; Headers = []}
    let ctx = {Request = req; Response = res}
    executeWebPart ctx webpart
  | _ -> printfn "invalid request. Request Format {HttpMethod};{Path}"
