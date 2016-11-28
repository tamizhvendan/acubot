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

let genericAppreciationMsgs = 
  [ "%s is on a roll!"
    "Keep going!"
    "Good work, %s!" 
    "I hope you are enjoying it, %s!"
    "Let's keep the pace up!"
    "Impressive %s!"
    "Impressive"
    "Keep Going %s!"]    

let steps  = [
  {
    Objective = "The objective of this challenge is to create a Discriminated Union Type to represent `HttpMethod`"
    QuickHint = "Discriminated unions are used to represent a finite, well-defined set of choices. They can be equated to enums in other programming languages."
    Asserts = [Compiler2 ("HttpMethod.Get","'HttpMethod'(Get) is not defined"); Compiler "Put"; Compiler "Delete"; Compiler "Post"]
    Appreciations = 
      [ "Great start, partner!"
        "It is a good start %s"]
  }
  {
    Objective = "Let's create a Pair Type to representing `Header`"
    QuickHint = """A tuple is defined as a comma separated collection of values. For example, ("key", "value") is a 2-tuple with the type (string * string). Tuples are extremely useful for creating ad hoc data structures which group together related values"""
    Asserts = 
      [ Compiler2 ("""let header : Header = ("foo", "bar")""", "The type `Header` should be of type 'string * string'") 
        Expression ("header", """("foo", "bar")""")]
    Appreciations = ["That was easy, wasn't it?"; "Hey, You got it right!"]
  }
  {
    Objective = "We need a new Record type `Request`"
    QuickHint = """Relational database theory uses a similar "record type" concept. In the relational model, a relation is a finite set of tuples all having the same finite set of attributes. This set of attributes is familiarly referred to as the set of column names."""
    Asserts = 
      [Compiler2 (req "test" "Get", 
          recordErrMsg "Request" [("Path","string"); ("Headers", "Header list"); ("HttpMethod", "HttpMethod")])]
    Appreciations = ["You did learn how to put things together, %s!"]
  }
  {
    Objective = "Now, we shall define Discrimintated Union Type to represent `StatusCode`"
    QuickHint = "Isn't it similar to what you did for `HttpMethod` in the first challenge?"
    Asserts = [Compiler "StatusCode.Ok"; Compiler "NotFound"; Compiler "BadRequest"]
    Appreciations = []
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
    Appreciations = []
  }
  {
    Objective = "We shall move on to modelling a `WebPart`"
    QuickHint = "A type can represent a function signature"
    Asserts = 
      [Compiler2 ("let _ : WebPart = fun ctx -> ctx |> Some |> async.Return", "WebPart should be of type `Context -> Async<Context option>`") ]
    Appreciations = ["Functions can also be first class citizens? That's amazing"]
  }
  {
    Objective = "Let's define our first Combinator `OK`"
    QuickHint = "A function that takes a string and returns a WebPart. (string -> WebPart)"
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
    QuickHint = "Can you duplicate what you did in the previous challenge with a change in `StatusCode`?"
    Asserts = 
      [Compiler2("""let _ : WebPart = NOT_FOUND "test";;""",
                  "The `NOT_FOUND` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("""run (NOT_FOUND "test")""",rawRes "NotFound" "test", "NOT_FOUND")
      ]
    Appreciations = []
  }
  {
    Objective = "Let's repeat that for `BAD_REQUEST` Combinator"
    QuickHint = "Can I assume that you don't need a hint here ʘ‿ʘ"
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
    QuickHint = "Rule of Three states that the code can be copied once, but that when the same code is used three times, it should be extracted into a new function"
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
    QuickHint = "It's time to put the Option type in action"
    Asserts = filterAsserts "GET" "Get" "Post" 
    Appreciations = ["So, `null` was nullified, ha ha! :)"]     
  }
  {
    Objective = "Now let's define `POST` Filter"
    QuickHint = "Isn't it similar to what you did in the previous challenge"
    Asserts = filterAsserts "POST" "Post" "Get"
    Appreciations = []
  }
  {
    Objective = "How about a `PUT` Filter"
    QuickHint = "I am sure you are hands on Ctrl+C right now!"
    Asserts = filterAsserts "PUT" "Put" "Get"
    Appreciations = []
  }
  {
    Objective = "and finally we shall implement a `DELETE` Filter"
    QuickHint = "Thinking of refactoring, hold a minute. It's a next challenge"
    Asserts = filterAsserts "DELETE" "Delete" "Get"
    Appreciations = []
  }
  {
    Objective = "Let us refactor Filter WebParts"
    QuickHint = "A time to refresh and duplicate what you did in the previous refactoring challenge"
    Asserts = 
      [Compiler2("""let t : WebPart = httpMethodFilter Get;;""",
                  "The `httpMethodFilter` function signature should be `Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("run t",rawRes "NotFound" "", "httpMethodFilter")
       ] @ (filterAsserts "GET" "Get" "Post")
       @ (filterAsserts "POST" "Post" "Get" )
       @ (filterAsserts "PUT" "Put" "Get")
       @ (filterAsserts "DELETE" "Delete" "Put")
    Appreciations = ["So %s, do you feel good about refactoring now?"]
  }
  {
    Objective = "Define `Path` filter"
    QuickHint = "This is a different filter. You need to check on Request's `Path` instead of `HttpMethod`"
    Asserts = 
      [Compiler2("""let t : WebPart = Path "/test";;""",
                  "The `Path` function signature should be `string -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("run t",rawRes "NotFound" "", "Path")]
    Appreciations = ["Not all filter are the same!"]
  }
  {
    Objective = "Define `compose` function"
    QuickHint = "WARNING! A Mind bending functional programming ahead (^0_0^)"
    Asserts = composeAssets "compose"
    Appreciations = ["Wow, I just had an epiphany!"; "That was an Eureka moment!"]
  }
  {
    Objective = "Define `>=>` function"
    QuickHint = "You can create alias to make you code short and succinct"
    Asserts = composeAssets "(>=>)"
    Appreciations = []
  }
  {
    Objective = "Define `Choose` function"
    QuickHint = "How a functional programming talk will end without recursion?"
    Asserts = 
      [Compiler2("""let t : WebPart = Choose [GET >=> OK "GET"; POST >=> OK "POST"];;""",
                  "The `Choose` function signature should be `WebPart list -> Context -> Async<Context option>`")
       Compiler(runWebPart)
       Expression2("run t",rawRes "Ok" "GET", "Choose")
       Compiler(runWebPart2 "Post")
       Expression2("run t",rawRes "Ok" "POST", "Choose")]
    Appreciations = ["Mission Complete! Well done %s"]
  }
]