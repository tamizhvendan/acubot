#r "packages/FAKE/tools/FakeLib.dll"
open Fake
let pickRandomMessage list =
  let random = new System.Random()
  let index = random.Next(0, List.length list)   
  List.item index list   

let green = trace
let yellow = traceFAKE
let whitefn = printfn
let white = printf

let red = traceError