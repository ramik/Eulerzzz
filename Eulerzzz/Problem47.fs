module Problem47

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open Primer

let removeEven x = 
    let rec removeEvenF y count = 
        match y % 2L with
        | 0L -> removeEvenF (y / 2L) (count + 1)
        | _ -> (y, count)
    removeEvenF x 0 
    
let findFactors candidate =
    let rec findFactorsF primes result = 
        let prime = Seq.head primes
        match prime, candidate with 
        | a, b when a > b -> result
        | a, b when b % a = 0.0 -> findFactorsF (Seq.skip 1 primes) (a :: result)
        | _ -> findFactorsF (Seq.skip 1 primes) result
    findFactorsF Primes []

let findResult =
    let rec findResultF count result x =
        let factors = findFactors x
        match Seq.length factors, count with
        | 4, 3 -> result
        | 4, 0 -> findResultF 1 x (x + 1.0)
        | 4, _ -> findResultF (count + 1) result (x + 1.0)
        | _, _ -> findResultF 0 0.0 (x + 1.0)

    findResultF 0 0.0 10.0

// 14 = 2 x 7
// 15 = 3 X 5

// 644 = 2² × 7 × 23
// 645 = 3 × 5 × 43
// 646 = 2 × 17 × 19