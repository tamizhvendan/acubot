#load "MiniSuave.fsx"
open MiniSuave

let ctx = {
  Request = { Method = Get; Path = "" }
  Response = {StatusCode = Ok; Content = ""}
}

let console = ConsoleWriter ctx