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