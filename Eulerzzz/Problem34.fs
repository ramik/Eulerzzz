module problem34
#if INTERACTIVE
#r @"C:\Eulerzzz\lib\xunit.dll"
#endif

open System
open Microsoft.FSharp.Math

let calculateFactorialSums x = x.ToString().ToCharArray() |> Seq.map (fun c -> Int32.Parse(c.ToString())) |> Seq.map (fun c -> (factorial c)) 
calculateFactorialSums 145 |> ignore

module problem34Unitests = 
  open Xunit


module printSolution = printfn "Solution's result is not done %A" [] 