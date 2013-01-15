module Problem47

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open Primer
open System

let rec getNextPrime x = 
    match x with
    | 1.0 -> 2.0
    | 2.0 -> 3.0
    | _ -> let candidate = x + 2.0
           if(isPrime candidate) then candidate
           else
                getNextPrime candidate   

let findFactors candidate =
    let rec findFactorsF x primeStub result = 
       if(isPrime x && x > primeStub) then (x :: result)
       else
           let prime = getNextPrime primeStub
           match prime > x, x % prime with
           | true, _ -> result
           | _, 0.0 -> findFactorsF (x / prime) prime (prime :: result)
           | _ -> findFactorsF x prime result
    if(isPrime(candidate)) then []
    else
        findFactorsF candidate 1.0 []

let findResult =
    let rec findResultF count result x =
        let factors = findFactors x
        match Seq.length factors, count with
        | 4, 4 -> result
        | 4, 1 -> findResultF (count + 1) x (x + 1.0)
        | 4, _ -> findResultF (count + 1) result (x + 1.0)
        | _, _ -> findResultF 1 0.0 (x + 1.0)

    findResultF 1 0.0 10.0