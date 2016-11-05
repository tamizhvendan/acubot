#load "MiniSuave.fsx"
#load "test.fsx"
open MiniSuave
open Test

let app =
  Choose[
    GET >=> Choose [
      Path "/hello" >=> OK "hello"
      Path "/hi" >=> OK "hi"
    ]
    POST >=> Path "/hello" >=> OK "hello POST"
  ]