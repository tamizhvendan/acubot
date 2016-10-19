#r "packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"

open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler
open System.IO
open System.Text

let sbOut = StringBuilder()
let sbErr = StringBuilder()

let fsiPath = "./packages/FSharp.Compiler.Tools/tools/fsi.exe"

let fsi =
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)
    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let argv = [|fsiPath|]
    FsiEvaluationSession.Create(fsiConfig, argv, inStream, outStream, errStream)

type Result<'T, 'E> =
| Success of 'T
| Error of 'E


let evalInteraction content = 
    let _, errs = fsi.EvalInteractionNonThrowing content
    if errs.Length > 0 then sprintf "%A" errs |> Error 
    else Success ()

let evalExpression content =
    let res,errs = fsi.EvalExpressionNonThrowing content
    if errs.Length > 0 then 
        errs 
        |> Array.map (fun e -> e.Message) 
        |> Array.reduce (fun v1 v2 -> v1 + "," + v2)
        |> Error
    else 
        match res with
        | Choice1Of2 x -> Success x
        | Choice2Of2 ex -> Error ex.Message 
