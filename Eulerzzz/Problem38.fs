module Problem38

open System 
let multipliers = { 1..9 }
let getConcanation x = 
  let rec getConcat x result = 
    match String.length result, x with
      | 9, _ -> Int32.Parse result
      | a, hd :: tl when a < 10 -> getConcat tl (result + hd.ToString()) 
      | a, _ when a > 9 -> 0 

  getConcat x ""
let isDistinct x = x.ToString().ToCharArray() |> Seq.filter (fun c -> c <> '0') |> Seq.distinct |> Seq.length 
let getProducts (a,b) = b |> Seq.map (fun c -> a*c) |> Seq.toList
let result = Seq.initInfinite (fun c -> (c + 1, multipliers)) 
                |> Seq.takeWhile (fun (c, _) -> c < 987654)
                |> Seq.map (fun c -> getProducts c) |> Seq.map getConcanation 
                |> Seq.filter (fun c -> c > 0)
                |> Seq.filter (fun c -> isDistinct c = 9)
                |> Seq.max
                 