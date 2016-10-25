// Show your Awesomeness!

type HttpMethod = 
| Get 
| Put 
| Delete 
| Post

type Header = string * string

type Request = {
  Path : string
  Headers : Header list
  HttpMethod : HttpMethod
}

type StatusCode =
| Ok
| BadRequest
| NotFound

type Response = {
  Content : string
  Headers : Header list
  StatusCode : StatusCode
}

type Context = {
  Request : Request
  Response : Response
}