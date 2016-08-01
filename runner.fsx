type HttpMethod = Create | Delete | Get | Put | Post

type Request = {
  Method : HttpMethod
  Path : string
}