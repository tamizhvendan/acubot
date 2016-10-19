type Assert =
| Compiler of string

type Step = {
  Id : int
  Description : string
  Greeting : string
  Asserts : Assert list
}

let steps  = [
  {
    Id = 1
    Description = "Create a Discrimintated Union Type to represent `HttpMethod`"
    Greeting = "That's a good start!"
    Asserts = [Compiler "HttpMethod.Get"; Compiler "Put"; Compiler "Delete"; Compiler "Post"]
  }
]