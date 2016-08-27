#load "MiniSuave.fsx"
#load "test.fsx"
open MiniSuave
open Test

let handlers = 
  Choose[
    GET >=> Choose [
      Path "/hello" >=> OK "hello"
      Path "/hi" >=> OK "hi"
    ]
    POST >=> Path "/hello" >=> OK "hello POST"
  ]

let app req = server req handlers
