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

type WebPart = Context -> Async<Context option>

let OK content ctx = 
  let response = 
    {ctx.Response with 
      StatusCode = Ok
      Content = content}  
  {ctx with Response = response} |> Some |> async.Return