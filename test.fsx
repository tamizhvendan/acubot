#load "MiniSuave.fsx"
open MiniSuave

let ctx = {
  Request = { HttpMethod = Get; Path = "" }
  Response = {StatusCode = Ok; Content = ""}
}

let console = ConsoleWriter ctx

let parseRequest (input : System.String) =
  let parts = input.Split([|';'|])
  let rawType = parts.[0]
  let route = parts.[1]
  match rawType with
  | "GET" -> {HttpMethod = Get; Path = route}
  | "POST" -> {HttpMethod = Post; Path = route}
  | "PUT" -> {HttpMethod = Put; Path = route}
  | _ -> failwith "invalid request"

let server text =
  let req = parseRequest text
  let res = {StatusCode = Ok; Content = ""}
  ConsoleWriter {Request = req; Response = res}