module Problem47

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open Primer
open System
    
let findFactors candidate =
    let rec findFactorsF prime result = 
        if(isPrime(prime) = false) then findFactorsF (prime + 2.0) result
        else
            match prime, candidate with 
            | 2.0, a when a % 2.0 = 0.0 -> findFactorsF 3.0 (2.0 :: result)
            | 2.0, _ -> findFactorsF 3.0 result
            | a, b when a > b / 2.0 -> result
            | a, b when b % a = 0.0 -> findFactorsF (candidate / prime) (a :: result)
            | _ -> findFactorsF (prime + 2.0) result
    findFactorsF 2.0 []

let findResult =
    let rec findResultF count result x =
        let factors = findFactors x
        if(x % 1000.0 = 0.0) then printfn "%A %A %A" count result x 
        match Seq.length factors, count with
        | 3, 3 -> result
        | 3, 1 -> findResultF (count + 1) x (x + 1.0)
        | 3, _ -> findResultF (count + 1) result (x + 1.0)
        | _, _ -> findResultF 1 0.0 (x + 1.0)

    findResultF 1 0.0 10.0

// 14 = 2 x 7
// 15 = 3 X 5

// 644 = 2² × 7 × 23
// 645 = 3 × 5 × 43
// 646 = 2 × 17 × 19