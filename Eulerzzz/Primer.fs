module Primer

(*
#if INTERACTIVE
#r @"..\lib\xunit.dll"
#endif
*)
open System

let isPrime x = let rec Sieve x y = match x % y, sqrt x < y with
                                                 | (0.0, _) -> false           
                                                 | (_, true) -> true
                                                 | _ -> Sieve x (y + 2.0)
                match x, x % 2.0 with 
                      | (a, _) when a < 2.0 -> false
                      | (2.0, _) -> true
                      | (_, 0.0) -> false
                      | (3.0, _) -> true
                      |  _ -> Sieve x 3.0

let private PrimesGreaterThan3 = seq { for i in 3.0 .. 2.0 .. System.Double.MaxValue do if(isPrime i) then yield i }
let Primes = Seq.append ([2.0] |> List.toSeq) PrimesGreaterThan3

module private PrimerUnitTests =
  open Xunit

  let ReturnsCorrect5items = 
    Assert.Equal([2.0;3.0;5.0;7.0;11.0;13.0;17.0], Primes |> Seq.take 7 |> Seq.toList)
 
  let ReturnsCorrect7to12items = 
    Assert.Equal([19.0;23.0;29.0;31.0;37.0], Primes |> Seq.skip 7 |> Seq.take 5 |> Seq.toList)

  let ReturnsCorrectWhenitems = 
    Assert.Equal([2.0;3.0;5.0;7.0;11.0;13.0;17.0], Primes |> Seq.takeWhile (fun x -> x < 18.0) |> Seq.toList)