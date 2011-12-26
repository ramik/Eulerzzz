module Problem37

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open System
open Primer

let primeToStringitems x = x.ToString().ToCharArray() |> Array.map (fun c -> c.ToString()) |> Array.toList

let private getTruncations x =
  let rec getTruncs x result tailfunc =
      match x with 
        | _ :: [] -> result
        | _ :: tl -> let value = tailfunc tl
                     getTruncs tl (value :: result) tailfunc
        | [] -> failwith "ei pitäisi tulla tänne"
  let rigthToLeft = getTruncs (List.rev x) [] (fun c -> String.Concat (List.rev c) |> Double.Parse)
  let leftToRight = getTruncs x [] (fun c -> String.Concat c |> Double.Parse)
  let value = String.Concat x |> Double.Parse
  value :: leftToRight @ rigthToLeft

let isTruncatedPrime = primeToStringitems >> getTruncations >> List.forall isPrime
let result = Primes |> Seq.skipWhile (fun c -> c < 10.0) |> Seq.filter isTruncatedPrime |> Seq.take 11 |> Seq.sum

module problem37Unitests = 
  open Xunit

  let primeToStringItems =
    Assert.Equal(["3";"7";"9";"7"], primeToStringitems 3797)

  let getTruncationsFrom3797 = 
    Assert.Equal([3797.0;7.0;97.0;797.0;3.0;37.0;379.0], getTruncations ["3";"7";"9";"7"])
  
  let getTruncationsFrom12345 = 
    Assert.Equal([12345.0;5.0;45.0;345.0;2345.0;1.0;12.0;123.0;1234.0], getTruncations ["1";"2";"3";"4";"5"])