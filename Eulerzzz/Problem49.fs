module Problem49

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open Primer
open System
        
let isPermutation x y = 
    let first = x.ToString().ToCharArray() |> Seq.toList |> List.sort  
    let second = y.ToString().ToCharArray() |> Seq.toList |> List.sort 
    first = second

let isValidFor x y = isPermutation x y && isPrime(x) && isPrime(y)
       
let findSolution() = 
    let rec findSolF x result =
        let y = x + 3330.0
        let z = x + 2.0 * 3330.0
        if(z > 9999.0) then result
        else 
            match isValidFor x y, isValidFor y z with
            | true, true -> findSolF (x + 2.0) ((x, y, z) :: result)
            | _ -> findSolF (x + 2.0) result
    findSolF 1489.0 []

let result = findSolution() 
                |> Seq.collect (fun (a, b, c) -> (a.ToString() + b.ToString() + c.ToString())) 
                |> (fun c -> String.Join("", c)) 

