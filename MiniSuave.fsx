type HttpMethod = Get | Put | Delete | Post

type Header = string * string

type Request = {
  HttpMethod : HttpMethod
  Headers : Header list
  Path : string
}

type StatusCode = Ok | NotFound | BadRequest

type Response = {
  StatusCode : StatusCode
  Headers : Header list
  Content : string
}
