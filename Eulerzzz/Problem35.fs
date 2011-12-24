module problem35
#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open System
open Primer

let getCyclicItems i =
  let getCyclicTemplate x = 
    let firstitems = x |> Seq.take ((Seq.length x) - 1) 
    Seq.append x firstitems
  let toarray = (i.ToString().ToCharArray() |> Array.toSeq)
  let cyclic = getCyclicTemplate toarray
  Seq.windowed (Seq.length toarray) cyclic |> Seq.map (fun c -> String.Join("", c)) |> Seq.map (fun c -> Int32.Parse(c))

let isCyclic x = Seq.length (getCyclicItems x |> Seq.map (fun c -> (float) c) |> Seq.filter isPrime) = (x.ToString().ToCharArray() |> Array.length)
let getCyclics = Primes |> Seq.takeWhile (fun c -> c < 1000000.0) |> Seq.filter isCyclic

module problem35Unitests = 
  open Xunit

  let getCyclicItemsReturnCorrectItems = 
    Assert.Equal([197;971;719], getCyclicItems 197 |> Seq.toList)
    Assert.Equal([1234;2341;3412;4123], getCyclicItems 1234 |> Seq.toList)
    
  let getCyclicItemsFromPrimesBelow15 = 
    let items = Primes |> Seq.takeWhile (fun c -> c < 15.0) |> Seq.filter isCyclic |> Seq.toList
    Assert.Equal([2.0;3.0;5.0;7.0;11.0;13.0], items)

  let getCyclicItemsFromPrimesBelow100ButGreaterThan15 = 
    let items = Primes |> Seq.takeWhile (fun c -> c < 100.0) |> Seq.skip 6 |> Seq.filter isCyclic |> Seq.toList
    Assert.Equal([17.0;31.0;37.0;71.0;73.0;79.0;97.0], items)

  let getCyclicItemsFromPrimesBelow100 = 
    let items = Primes |> Seq.takeWhile (fun c -> c < 100.0) |> Seq.filter isCyclic |> Seq.toList
    Assert.Equal(13, items |> List.length)
    
module printSolution =
  printfn "Solution's result is %i" (getCyclics |> Seq.length)