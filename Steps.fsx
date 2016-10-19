type Assert =
| Compiler of string
| Expression of string * string

type Step ={
  Description : string
  Greeting : string
  Asserts : Assert list
}

let steps  = [
  {
    Description = "Create a Discrimintated Union Type to represent `HttpMethod`"
    Greeting = "That's a good start!"
    Asserts = [Compiler "HttpMethod.Get"; Compiler "Put"; Compiler "Delete"; Compiler "Post"]
  }
  {
    Description = "Create a Pair Type to representing `Header`"
    Greeting = "Superb!"
    Asserts = [Compiler ("""let header : Header = ("foo", "bar")"""); Expression ("header", "")]
  }
]