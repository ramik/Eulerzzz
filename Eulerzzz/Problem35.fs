module problem35
#if INTERACTIVE
#r @"C:\Eulerzzz\lib\xunit.dll"
#endif

open System

let getCyclicItems i =
  let getCyclicTemplate x = 
    let firstitems = x |> Seq.take ((Seq.length x) - 1) 
    Seq.append x firstitems
  let toarray = (i.ToString().ToCharArray() |> Array.toSeq)
  let cyclic = getCyclicTemplate toarray
  Seq.windowed ((Seq.length toarray) - 1) cyclic


let a = 179

module problem35Unitests = 
  open Xunit

  let sumsUpCorrectlyWhenBelow10 = 
    Assert.Equal(23, 23)
    
module printSolution =
  printfn "Solution's result is %s" "not done"