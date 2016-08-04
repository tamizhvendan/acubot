type Asserts =
| Compiler of string list
| Value of (string * string) list
| Output of (string * string)

type Step = {
  Id : int
  Description : string
  Asserts : Asserts
  Expressions : string list
  Message : string
}

type Runner = {
  Greeting : string
  End : string
  Steps : Step list
}

let res2 =
  sprintf """let %s = {StatusCode = %s; Content = "%s"}"""
let res = res2 "res"

let req2 =
  sprintf """let %s = {Method = %s; Path = "%s"}"""
let req = req2 "req"

let ctx2 =
  sprintf """let %s = {Request = %s; Response = %s}"""

let ctx = ctx2 "ctx"

let ctx3 = [
  req "Get" "foo"
  res "Ok" "foo"
  ctx "req" "res"
]
let (++) = List.append

let handlerExpressions handler statusCode =
  ctx3 ++ [ res2 "expectedRes" statusCode "test"
            ctx2 "expectedCtx" "req" "expectedRes"
            "let expected = Some expectedCtx"
            sprintf """let result = %s "test" ctx""" handler]

let steps = [
  {
    Id = 1
    Description = "Create a Discrimintated Union Type to represent `HttpMethod`"
    Expressions = []
    Message = "That's a good start!"
    Asserts = Compiler ["HttpMethod.Get"; "Put"; "Post"]
  }
  {
    Id = 2
    Description = "Create a Record type for modeling `Request`"
    Expressions = []
    Message = "Great! Let's move on"
    Asserts = Compiler [req "Get" "foo"]
  }
  {
    Id = 3
    Description = "Create a Discrimintated Union Type to represent HTTP `StatusCode`"
    Expressions = []
    Message = "Awesome! Keep going!!"
    Asserts = Compiler ["StatusCode.Ok"; "NotFound"; "BadRequest"]
  }
  {
    Id = 4
    Description = "Define a function to return the HTTP status code"
    Expressions = []
    Message = "Wow! That's your first function"
    Asserts = Value [
                      ("toStatusCode Ok", "200")
                      ("toStatusCode NotFound", "404")
                      ("toStatusCode BadRequest", "400")]
  }
  {
    Id = 5
    Description = "Create a Record type for modeling `Response`"
    Expressions = []
    Message = "Great!"
    Asserts = Compiler [res "Ok" "foo"]
  }
  {
    Id = 6
    Description = "Create a Record type for modeling `Context`"
    Expressions = [res "Ok" "foo";req "Get" "foo"]
    Message = "Fantastic!"
    Asserts = Compiler [ctx "req" "res"]
  }
  {
      Id = 7
      Description = "Create a Handler type modeling handler function"
      Expressions = ctx3
      Message = "Welcome to functional programming!"
      Asserts = Compiler ["let test : Handler = Some"]
  }
  {
    Id = 8
    Description = "Create a `OK` handler"
    Expressions = handlerExpressions "OK" "Ok"
    Message = "Fantastic!"
    Asserts = Value ["result", "expected"]
  }
  {
    Id = 9
    Description = "Create a `ConsoleWriter` function"
    Expressions = ctx3
    Message = "Fantastic!"
    Asserts = Output ("""ConsoleWriter ctx (OK "test")""", "StatusCode: 200\nContent : test\n")
  }
  {
    Id = 10
    Description = "Create a `NOT_FOUND` handler"
    Expressions = handlerExpressions "NOT_FOUND" "NotFound"
    Message = "Fantastic!"
    Asserts = Value ["result", "expected"]
  }
]

let runner = {
  Greeting = "Welcome to Mini-Suave Workshop"
  End = "You made it!"
  Steps = steps
}