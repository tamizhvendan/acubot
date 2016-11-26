type Assert =
| Compiler of string
| Compiler2 of string * string
| Expression of string * string
| Expression2 of string * string * string

type Step ={
  Objective : string
  QuickHint : string
  Asserts : Assert list
  Appreciations: string list
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
    Objective = "The objective of this challenge is to create a Discriminated Union Type to represent `HttpMethod`"
    QuickHint = "Discriminated unions are used to represent a finite, well-defined set of choices. They can be equated to enums in other programming languages."
    Asserts = [Compiler2 ("HttpMethod.Get","'HttpMethod'(Get) is not defined"); Compiler "Put"; Compiler "Delete"; Compiler "Post"]
    Appreciations = 
      ["Great start, partner!"
       "It is a good start %s"]
  }
  {
    Objective = "Let's create a Pair Type to representing `Header`"
    QuickHint = "Superb!"
    Asserts = 
      [ Compiler2 ("""let header : Header = ("foo", "bar")""", "The type `Header` should be of type 'string * string'") 
        Expression ("header", """("foo", "bar")""")]
    Appreciations = ["That was easy, wasn't it?"; "Hey, You got it right!"]
  }
  {
    Objective = "We need a new Record type `Request`"
    QuickHint = ""
    Asserts = 
      [Compiler2 (req "test" "Get", 
          recordErrMsg "Request" [("Path","string"); ("Headers", "Header list"); ("HttpMethod", "HttpMethod")])]
    Appreciations = ["You did learn how to put things together, %s!"; "Keep going!"]
  }
  {
    Objective = "Now, we shall define Discrimintated Union Type to represent `StatusCode`"
    QuickHint = "Isn't it similar to what you did for `HttpMethod` in the first challenge?"
    Asserts = [Compiler "StatusCode.Ok"; Compiler "NotFound"; Compiler "BadRequest"]
    Appreciations = ["%s is on a roll!"; "Let's keep the pace up!"]
  }
  {
    Objective = "A Record type `Response`, please?"
    QuickHint = "May be you should do it the same way you did for `Request`?"
    Asserts = 
      [Compiler2 (res "test" "Ok", 
          recordErrMsg "Response" [("Content","string"); ("Headers", "Header list"); ("StatusCode", "StatusCode")])]
    Appreciations = ["......and now we have `Response`. Great!"; "You had request and now you have Response too."]
  }
  {
    Objective = "Now let us make a Record type `Context`"
    QuickHint = "You can keep piling types to form newer ones."
    Asserts = 
      [Compiler2 (ctx (req "test" "Get") (res "test" "Ok"), 
          recordErrMsg "Context" [("Request","Request"); ("Response", "Response")])]
    Appreciations = ["Good work, %s!"; "I hope you are enjoying it, %s!"]
  }
  {
    Objective = "We shall move on to modelling a `WebPart`"
    QuickHint = "Incredible!"
    Asserts = 
      [Compiler2 ("let _ : WebPart = fun ctx -> ctx |> Some |> async.Return", "WebPart should be of type `Context -> Async<Context option>`") ]
    Appreciations = ["Functions can also be first class citizens? That's amazing"]
  }
  {
    Objective = "Let's define our first Combinator `OK`"
    QuickHint = "Wonderful!"
    Asserts = 
      [Compiler2("""let _ : WebPart = OK "test";;""",
                  "The `OK` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("""run (OK "test")""",rawRes "Ok" "test", "OK")
      ]
    Appreciations = ["OK! (pun intended)"; "Good going!"]
  }
  {
    Objective = "We shall define `NOT_FOUND` Combinator now"
    QuickHint = "Nice.."
    Asserts = 
      [Compiler2("""let _ : WebPart = NOT_FOUND "test";;""",
                  "The `NOT_FOUND` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("""run (NOT_FOUND "test")""",rawRes "NotFound" "test", "NOT_FOUND")
      ]
    Appreciations = ["Impressive %s!"; "Keep Going %s!"]
  }
  {
    Objective = "Let's repeat that for `BAD_REQUEST` Combinator"
    QuickHint = "Cool"
    Asserts = 
      [Compiler2("""let _ : WebPart = BAD_REQUEST "test";;""",
                  "The `BAD_REQUEST` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("""run (BAD_REQUEST "test")""",rawRes "BadRequest" "test", "BAD_REQUEST")
      ]
    Appreciations = ["Was that a repetitive?"]
  }
  {
    Objective = "It's time for refactoring"
    QuickHint = ""
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
       Expression2("""run (BAD_REQUEST "test")""",rawRes "BadRequest" "test", "BAD_REQUEST")]
    Appreciations = ["We changed boring to fun, didn't we %s?"; "Hi5! %s"]
  }
  {
    Objective = "Let's define `GET` Filter"
    QuickHint = "Wonderful"
    Asserts = filterAsserts "GET" "Get" "Post" 
    Appreciations = [""]     
  }
  {
    Objective = "Define `POST` Filter"
    QuickHint = "Superb"
    Asserts = filterAsserts "POST" "Post" "Get"
    Appreciations = []  
  }
  {
    Objective = "Define `PUT` Filter"
    QuickHint = "Keep Going!"
    Asserts = filterAsserts "PUT" "Put" "Get"
    Appreciations = []
  }
  {
    Objective = "Define `DELETE` Filter"
    QuickHint = "That's amazing!"
    Asserts = filterAsserts "DELETE" "Delete" "Get"
    Appreciations = []
  }
  {
    Objective = "Refactor Filter WebParts"
    QuickHint = "You are awesome!"
    Asserts = 
      [Compiler2("""let t : WebPart = httpMethodFilter Get;;""",
                  "The `httpMethodFilter` function signature should be `Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("run t",rawRes "NotFound" "", "httpMethodFilter")
       ] @ (filterAsserts "GET" "Get" "Post")
       @ (filterAsserts "POST" "Post" "Get" )
       @ (filterAsserts "PUT" "Put" "Get")
       @ (filterAsserts "DELETE" "Delete" "Put")
    Appreciations = []
  }
  {
    Objective = "Define `Path` filter"
    QuickHint = "Well done!"
    Asserts = 
      [Compiler2("""let t : WebPart = Path "/test";;""",
                  "The `Path` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("run t",rawRes "NotFound" "", "Path")]
    Appreciations = []
  }
  {
    Objective = "Define `compose` function"
    QuickHint = "Wow!"
    Asserts = composeAssets "compose"
    Appreciations = ["Wow, I just had an epiphany!"; "That was an Eureka moment!"]
  }
  {
    Objective = "Define `>=>` function"
    QuickHint = "Cool :-)"
    Asserts = composeAssets "(>=>)"
    Appreciations = []
  }
  {
    Objective = "Define `Choose` function"
    QuickHint = "That's it! Well done :-)"
    Asserts = 
      [Compiler2("""let t : WebPart = Choose [GET >=> OK "GET"; POST >=> OK "POST"];;""",
                  "The `Choose` function signature should be `WebPart list -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("run t",rawRes "Ok" "GET", "Choose")
       Compiler(runWebPart2 "Post")
       Expression2("run t",rawRes "Ok" "POST", "Choose")]
    Appreciations = []
  }
]