type Asserts =
| Compiler of string list
| Value of (string * string) list

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
    Asserts = Compiler ["""{Request.Method = Get; Path = "foo"}"""]
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
    Asserts = Compiler ["""{Response.StatusCode = Ok; Content = "foo"}"""]
  }
  {
    Id = 6
    Description = "Create a Record type for modeling `Context`"
    Expressions = [
                    """let res = {StatusCode = Ok; Content = "foo"}"""
                    """let req = {Method = Get; Path = "foo"}"""
    ]
    Message = "Fantastic!"
    Asserts = Compiler ["""{Request = req; Response = res}"""]
  }
  {
    Id = 7
    Description = "Create a `OK` function"
    Expressions = [
                    """let res = {StatusCode = Ok; Content = "foo"}"""
                    """let req = {Method = Get; Path = "foo"}"""
                    """let ctx = {Request = req; Response = res}"""
                    """let expectedRes = {res with Content = "test"}"""
                    """let expected = Some {ctx with Response = expectedRes}"""
                    """let result = OK "test" ctx"""
    ]
    Message = "Fantastic!"
    Asserts = Value ["result", "expected"]
  }
]

let runner = {
  Greeting = "Welcome to Mini-Suave Workshop"
  End = "You made it!"
  Steps = steps
}