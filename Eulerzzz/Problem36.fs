module Problem36

open System

let convertToBinary (n:int) = Convert.ToString(n,2).ToCharArray() |> Array.toList

let isPalindrome x = List.rev x = x
let bothPalindrome (x, y) = match x, y with
                              | (true, true) -> true
                              | _ -> false  
let is10BaseNumberPalindrome x = x.ToString().ToCharArray() |> Array.toList |> isPalindrome
let is2BaseNumberPalindrome x = convertToBinary x |> isPalindrome

let candidates = Seq.initInfinite (fun c -> c + 1)
let result x = x |> Seq.map (fun c -> c, (is10BaseNumberPalindrome c, is2BaseNumberPalindrome c))
                 |> Seq.filter (fun (_, c) -> bothPalindrome c)      

let test = candidates |> Seq.takeWhile (fun c -> c < 1000000) |> result |> Seq.sumBy (fun (c, _) -> c) 

