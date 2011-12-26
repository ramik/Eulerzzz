module Problem41

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open System 
open Primer

let isPandigital x = 
      let arr = x.ToString().ToCharArray() |> Array.toList |> List.map (fun c -> Int32.Parse (c.ToString()))
      let len = List.length arr
      List.sort arr = [1..len]

let result = Primes |> Seq.takeWhile (fun c -> c < 9876500.0) |> Seq.filter isPandigital |> Seq.max