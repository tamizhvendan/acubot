type Assert =
| Compiler of string
| Compiler2 of string * string
| Expression of string * string

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

let req = sprintf """{Path = "%s"; Headers = [("foo", "bar")]; HttpMethod= %s}"""
let res = sprintf """{Content = "%s"; Headers = [("foo", "bar")]; StatusCode= %s}"""
let ctx = sprintf """{Request = %s; Response = %s}"""

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
]