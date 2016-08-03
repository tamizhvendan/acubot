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

let OK content ctx =
  let res = {StatusCode = Ok; Content = content}
  Some {ctx with Response = res}