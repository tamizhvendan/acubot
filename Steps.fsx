type Step = {
  Id : int
  Description : string
  Asserts : string list
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
    Asserts = ["HttpMethod.Get"; "Put"; "Post"]
  }
  {
    Id = 2
    Description = "Create a Record type for modeling `Request`"
    Expressions = []
    Message = "Great! Let's move on"
    Asserts = ["""{Request.Method = Get; Path = "foo"}"""]
  }
]

let runner = {
  Greeting = "Welcome to Mini-Suave Workshop"
  End = "You made it!"
  Steps = steps
}