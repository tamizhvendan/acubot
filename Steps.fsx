type Assert =
| Compiler of string
| Compiler2 of string * string
| Expression of string * string
| Expression2 of string * string * string

type Step ={
  Description : string
  Greeting : string
  Asserts : Assert list
}

let recordErrMsg name labels =
  labels 
  |> List.map (fun (label, type') -> sprintf "`%s`(of type %s)" label type')
  |> List.reduce (fun v1 v2 -> v1 + "," + v2)
  |> sprintf "The Record type `%s` should contain labels: %s" name

let runWebPart = """
  let run webpart =
    let req = {HttpMethod = Get; Path = "/test"; Headers = []}
    let res = {StatusCode = NotFound; Content = ""; Headers = []}
    let ctx = {Request = req; Response = res}
    let result = webpart ctx |> Async.RunSynchronously
    match result  with
    | Some ctx ->
      sprintf "[%A]:%A" ctx.Response.StatusCode ctx.Response.Content 
    | None ->
       "<Empty>"
"""
let runWebPart2 httpMethod = runWebPart.Replace("Get", httpMethod)
let req = sprintf """{Path = "%s"; Headers = [("foo", "bar")]; HttpMethod= %s}"""
let res = sprintf """{Content = "%s"; Headers = [("foo", "bar")]; StatusCode= %s}"""

let rawRes s1 s2 = 
  (sprintf """"[%s]:"%s"" """ s1 s2).TrimEnd()

let emptyRes = (""""<Empty>" """).TrimEnd()

let ctx = sprintf """{Request = %s; Response = %s}"""
let filterAsserts webPart httpMethod negativeHttpMethod = 
  [Compiler2(sprintf """let _ : WebPart = %s;;""" webPart, 
      sprintf "The `%s` function signature should be `Context -> Async<Context option>`" webPart)
   Compiler(runWebPart2 negativeHttpMethod)
   Expression2("""run """ + webPart,emptyRes, webPart)
   Compiler(runWebPart2 httpMethod)
   Expression2("""run """ + webPart,rawRes "NotFound" "", webPart)
  ]

let composeAssets fname = 
       [Compiler2(sprintf """let t : WebPart = %s (OK "foo") (OK "bar") ;;"""  fname,
                  sprintf  "The `%s` function signature should be `WebPart -> WebPart -> Context -> Async<Context option>`" fname)
        Compiler(runWebPart)
        Expression2("run t",rawRes "Ok" "bar", fname)
        Compiler(sprintf """let x = %s POST (OK "bar")""" fname)
        Expression2("run x", emptyRes, fname)
        Compiler(sprintf """let y = %s (OK "bar") GET""" fname)
        Expression2("run y", rawRes "Ok" "bar", fname)]
        

let steps  = [
  {
    Description = "Create a Discrimintated Union Type to represent `HttpMethod`"
    Greeting = "That's a good start!"
    Asserts = [Compiler "HttpMethod.Get"; Compiler "Put"; Compiler "Delete"; Compiler "Post"]
  }
  {
    Description = "Create a Pair Type to representing `Header`"
    Greeting = "Superb!"
    Asserts = 
      [ Compiler2 ("""let header : Header = ("foo", "bar")""", "The type `Header` should be of type 'string * string'") 
        Expression ("header", """("foo", "bar")""")]
  }
  {
    Description = "Create a Record type `Request`"
    Greeting = "Keep going!"
    Asserts = 
      [Compiler2 (req "test" "Get", 
          recordErrMsg "Request" [("Path","string"); ("Headers", "Header list"); ("HttpMethod", "HttpMethod")])]
  }
  {
    Description = "Create a Discrimintated Union Type to represent `StatusCode`"
    Greeting = "That's amazing!"
    Asserts = [Compiler "StatusCode.Ok"; Compiler "NotFound"; Compiler "BadRequest"]
  }
  {
    Description = "Create a Record type `Response`"
    Greeting = "Awesome!"
    Asserts = 
      [Compiler2 (res "test" "Ok", 
          recordErrMsg "Response" [("Content","string"); ("Headers", "Header list"); ("StatusCode", "StatusCode")])]
  }
  {
    Description = "Create a Record type `Context`"
    Greeting = "Brilliant!"
    Asserts = 
      [Compiler2 (ctx (req "test" "Get") (res "test" "Ok"), 
          recordErrMsg "Context" [("Request","Request"); ("Response", "Response")])]
  }
  {
    Description = "Model `WebPart`"
    Greeting = "Incredible!"
    Asserts = 
      [Compiler2 ("let _ : WebPart = fun ctx -> ctx |> Some |> async.Return", "WebPart should be of type `Context -> Async<Context option>`") ]
  }
  {
    Description = "Define our first Combinator `OK`"
    Greeting = "Wonderful!"
    Asserts = 
      [Compiler2("""let _ : WebPart = OK "test";;""",
                  "The `OK` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("""run (OK "test")""",rawRes "Ok" "test", "OK")
      ]
  }
  {
    Description = "Define `NOT_FOUND` Combinator"
    Greeting = "Nice.."
    Asserts = 
      [Compiler2("""let _ : WebPart = NOT_FOUND "test";;""",
                  "The `NOT_FOUND` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("""run (NOT_FOUND "test")""",rawRes "NotFound" "test", "NOT_FOUND")
      ]
  }
  {
    Description = "Define `BAD_REQUEST` Combinator"
    Greeting = "Cool"
    Asserts = 
      [Compiler2("""let _ : WebPart = BAD_REQUEST "test";;""",
                  "The `BAD_REQUEST` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("""run (BAD_REQUEST "test")""",rawRes "BadRequest" "test", "BAD_REQUEST")
      ]
  }
  {
    Description = "It's time for refactoring"
    Greeting = "Great.."
    Asserts = 
      [Compiler2("""let _ : WebPart = response Ok "test";;""",
                  "The `response` function signature should be `StatusCode -> string -> Context -> Async<Context option>`")      
       Compiler2("""let _ : WebPart = OK "test";;""",
                  "The `OK` function signature should be `string -> Context -> Async<Context option>`")
       Compiler2("""let _ : WebPart = BAD_REQUEST "test";;""",
                  "The `BAD_REQUEST` function signature should be `string -> Context -> Async<Context option>`")
       Compiler2("""let _ : WebPart = NOT_FOUND "test";;""",
                  "The `NOT_FOUND` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("""run (response Ok "test")""",rawRes "Ok" "test", "response function")
       Expression2("""run (OK "test")""",rawRes "Ok" "test", "OK")
       Expression2("""run (NOT_FOUND "test")""",rawRes "NotFound" "test", "NOT_FOUND")
       Expression2("""run (BAD_REQUEST "test")""",rawRes "BadRequest" "test", "BAD_REQUEST")
      ]
  }
  {
    Description = "Define `GET` Filter"
    Greeting = "Wonderful"
    Asserts = filterAsserts "GET" "Get" "Post"      
  }
  {
    Description = "Define `POST` Filter"
    Greeting = "Superb"
    Asserts = filterAsserts "POST" "Post" "Get"      
  }
  {
    Description = "Define `PUT` Filter"
    Greeting = "Keep Going!"
    Asserts = filterAsserts "PUT" "Put" "Get"      
  }
  {
    Description = "Define `DELETE` Filter"
    Greeting = "That's amazing!"
    Asserts = filterAsserts "DELETE" "Delete" "Get"      
  }
  {
    Description = "Refactor Filter WebParts"
    Greeting = "You are awesome!"
    Asserts = 
      [Compiler2("""let t : WebPart = httpMethodFilter Get;;""",
                  "The `httpMethodFilter` function signature should be `Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("run t",rawRes "NotFound" "", "httpMethodFilter")
       ] @ (filterAsserts "GET" "Get" "Post")
       @ (filterAsserts "POST" "Post" "Get" )
       @ (filterAsserts "PUT" "Put" "Get")
       @ (filterAsserts "DELETE" "Delete" "Put")
  }
  {
    Description = "Define `Path` filter"
    Greeting = "Well done!"
    Asserts = 
      [Compiler2("""let t : WebPart = Path "/test";;""",
                  "The `Path` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("run t",rawRes "NotFound" "", "Path")]
  }
  {
    Description = "Define `compose` function"
    Greeting = "Wow!"
    Asserts = composeAssets "compose"
  }
  {
    Description = "Define `>=>` function"
    Greeting = "Cool :-)"
    Asserts = composeAssets "(>=>)"
  }
  {
    Description = "Define `Choose` function"
    Greeting = "That's it! Well done :-)"
    Asserts = 
      [Compiler2("""let t : WebPart = Choose [GET >=> OK "GET"; POST >=> OK "POST"];;""",
                  "The `Choose` function signature should be `WebPart list -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("run t",rawRes "Ok" "GET", "Choose")
       Compiler(runWebPart2 "Post")
       Expression2("run t",rawRes "Ok" "POST", "Choose")]
  }
]