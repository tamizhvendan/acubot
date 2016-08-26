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
  sprintf """let %s = {HttpMethod = %s; Path = "%s"}"""
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
    Description = "Its time for filters! Let's start from GET"
    Expressions = filterHandlerExpressions "Get" "GET"
    Message = "Amazing!"
    Asserts =
      ExecuteValue [
        [expected "Some ctx";result "GET"], ("result", "expected")
        [req "Post" "foo"; ctx "req" "res"; expected "None";result "GET"], ("result", "expected")]
  }
  {
    Id = 14
    Description = "Create a POST filter!"
    Expressions = filterHandlerExpressions "Post" "POST"
    Message = "That's Awesome!"
    Asserts =
      ExecuteValue [
        [expected "Some ctx";result "POST"], ("result", "expected")
        [req "Get" "foo"; ctx "req" "res"; expected "None";result "POST"], ("result", "expected")]
  }
  {
    Id = 15
    Description = "Create a PUT filter!"
    Expressions = filterHandlerExpressions "Put" "PUT"
    Message = "Cool!"
    Asserts =
      ExecuteValue [
        [expected "Some ctx";result "PUT"], ("result", "expected")
        [req "Get" "foo"; ctx "req" "res"; expected "None";result "PUT"], ("result", "expected")]
  }
  {
    Id = 16
    Description = "It's time for refactoring!"
    Expressions = ctx3
    Message = "Making great progress!"
    Asserts =
      ExecuteValue [
        [expected "Some ctx"; result "httpMethodFilter Get"], ("result", "expected")
        [req "Get" "foo"; ctx "req" "res"; expected "None";result "PUT"], ("result", "expected")
        [req "Post" "foo"; ctx "req" "res"; expected "None";result "PUT"], ("result", "expected")
        [req "Get" "foo"; ctx "req" "res"; expected "None";result "POST"], ("result", "expected")
        [req "Put" "foo"; ctx "req" "res"; expected "None";result "POST"], ("result", "expected")
        [req "Post" "foo"; ctx "req" "res"; expected "None";result "GET"], ("result", "expected")
        [req "Put" "foo"; ctx "req" "res"; expected "None";result "GET"], ("result", "expected")]
  }
  {
    Id = 17
    Description = "What about a Path filter?"
    Expressions = ctx3
    Asserts =
      ExecuteValue [
        [req "Get" "/foo"; ctx "req" "res"; expected "None";result """Path "/test" """], ("result", "expected")
        [req "Get" "/foo"; ctx "req" "res"; expected "Some ctx";result """Path "/foo" """], ("result", "expected")
      ]
    Message = "Great work!"
  }
  {
    Id = 18
    Description = "let's compose!"
    Expressions = ctx3 ++ [
                            """let app = compose (OK "test") (OK "foo")"""
                            res2 "expecteRes" "Ok" "foo"
                            ctx2 "ctx2" "req" "expecteRes"
                            ]
    Message = "Awesomeness!"
    Asserts = ExecuteValue[
                [expected "Some ctx2";result "app"], ("result", "expected")
              ]
  }
  {
    Id = 19
    Description = "You can make it even shorter!"
    Expressions = ctx3
    Message = "That's cool!"
    Asserts = Value ["""((OK "test") >=> (OK "foo")) ctx""", """(compose (OK "test") (OK "foo")) ctx"""]
  }
  {
    Id = 20
    Description = "Let's make it complex"
    Expressions = ctx3 ++ [
                            """let app = Choose [GET >=> OK "test"; POST >=> OK "post"]"""
                            res2 "expecteRes" "Ok" "test"]
    Message = "Yes! You are doing 'fun'ctional programming"
    Asserts =
      ExecuteValue[
        [ req "Get" ""
          ctx2 "expecteCtx" "req" "expecteRes"
          ctx "req" "res"
          expected "Some expecteCtx"
          result "app"], ("result", "expected")
        [ req "Post" ""
          ctx "req" "res"
          res2 "expecteRes" "Ok" "post"
          ctx2 "expecteCtx" "req" "expecteRes"
          expected "Some expecteCtx"
          result "app"], ("result", "expected")
        [ req "Put" ""
          ctx "req" "res"
          expected "None"
          result "app"], ("result", "expected")
      ]
  }
]

let runner = {
  Greeting = "Welcome to Mini-Suave Workshop"
  End = "You made it!"
  Steps = steps
}