module Problem37

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open System

let primeToStringitems x = x.ToString().ToCharArray() |> Array.map (fun c -> c.ToString()) |> Array.toList

let private getTruncations x =
  let rec getTruncs x result tailfunc =
      match x with 
        | _ :: [] -> result
        | _ :: tl -> let value = tailfunc tl
                     getTruncs tl (value :: result) tailfunc
        | [] -> failwith "ei pitäisi tulla tänne"
  let rigthToLeft = getTruncs (List.rev x) [] (fun c -> String.Concat (List.rev c) |> Int64.Parse)
  let leftToRight = getTruncs x [] (fun c -> String.Concat c |> Int64.Parse)
  let value = String.Concat x |> Int64.Parse
  value :: leftToRight @ rigthToLeft

module problem37Unitests = 
  open Xunit

  let primeToStringItems =
    Assert.Equal(["3";"7";"9";"7"], primeToStringitems 3797)

  let getTruncationsFrom3797 = 
    Assert.Equal([3797L;7L;97L;797L;3L;37L;379L], getTruncations ["3";"7";"9";"7"])
  
  let getTruncationsFrom12345 = 
    Assert.Equal([12345L;5L;45L;345L;2345L;1L;12L;123L;1234L], getTruncations ["1";"2";"3";"4";"5"])