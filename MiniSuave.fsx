type HttpMethod = Create | Delete | Get | Post | Put

type Request = {
  Method : HttpMethod

  Path : string
}

type StatusCode = Ok | NotFound | BadRequest

let toStatusCode = function
| Ok -> 200
| NotFound -> 404
| BadRequest -> 400

type Response = {
  StatusCode : StatusCode
  Content : string
}

type Context = {
  Request : Request
  Response : Response
}

type Handler = Context -> Context option

let responseHandler statusCode content ctx =
  let res = {StatusCode = statusCode; Content = content}
  Some {ctx with Response = res}

let OK = responseHandler Ok

let ConsoleWriter ctx handler =
  match handler ctx with
  | Some ctx ->
    ctx.Response.StatusCode
    |> toStatusCode
    |> printfn "StatusCode: %d"
    printfn "Content : %s" ctx.Response.Content
  | None ->
    printfn "Empty"

let NOT_FOUND = responseHandler NotFound

let BAD_REQUEST = responseHandler BadRequest

let filter httpMethod ctx =
  match ctx.Request.Method = httpMethod with
  | true -> Some ctx
  | _ -> None

let GET = filter Get
let POST = filter Post
let PUT = filter Put
let Path path ctx =
  match ctx.Request.Path = path with
  | true -> Some ctx
  | _ -> None

let compose h1 h2 ctx =
  match h1 ctx with
  | Some ctx -> h2 ctx
  | None -> None

let (>=>) = compose

let rec Choose webparts context =
  match webparts with
  | [] -> None
  | x :: xs ->
    let result = x context
    match result with
    | Some x -> Some x
    | None -> Choose xs context