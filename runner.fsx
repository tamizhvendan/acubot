type HttpMethod = Create | Delete | Get | Put | Post

type Request = {
  Method : HttpMethod
  Path : string
}

type StatusCode = Ok | NotFound | BadRequest

let toStatusCode = function
| Ok -> 200
| NotFound -> 404
| BadRequest -> 400