type Asserts =
| Compiler of string list
| Value of (string * string) list
| ExecuteValue of (string list * (string * string)) list
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
let (<+>) = sprintf "%s\n%s"

let result = sprintf "let result = %s ctx"
let expected = sprintf "let expected : Context option = %s"

let responseHandlerExpressions handler statusCode =
  ctx3 ++ [ res2 "expectedRes" statusCode "test"
            ctx2 "expectedCtx" "req" "expectedRes"
            expected "Some expectedCtx"
            sprintf """let result = %s "test" ctx""" handler]

let filterHandlerExpressions httpMethod handler =
  [ req httpMethod "foo"
    res "Ok" "foo"
    ctx "req" "res"]

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
    Asserts = Value [ ("toStatusCode Ok", "200")
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
    Expressions = responseHandlerExpressions "OK" "Ok"
    Message = "Keep going!"
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
    Expressions = responseHandlerExpressions "NOT_FOUND" "NotFound"
    Message = "Fantastic!"
    Asserts = Value ["result", "expected"]
  }
  {
    Id = 11
    Description = "Create a `BAD_REQUEST` handler"
    Expressions = responseHandlerExpressions "BAD_REQUEST" "BadRequest"
    Message = "Fantastic!"
    Asserts = Value ["result", "expected"]
  }
  {
    Id = 12
    Description = "Now it's time for refactoring and partial application!"
    Expressions = responseHandlerExpressions "responseHandler BadRequest" "BadRequest"
    Message = "Good. You are getting a grip on functional programming!"
    Asserts = Value ["result", "expected"]
  }
  {
    Id = 13
    Description = "Let's start writing filters!"
    Expressions = filterHandlerExpressions "Get" "GET"
    Message = "Amazing!"
    Asserts =
      ExecuteValue [
        [expected "Some ctx";result "GET"], ("result", "expected")
        [req "Post" "foo"; ctx "req" "res"; expected "None";result "GET"], ("result", "expected")]
  }
]

let runner = {
  Greeting = "Welcome to Mini-Suave Workshop"
  End = "You made it!"
  Steps = steps
}