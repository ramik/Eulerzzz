module Problem46

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open Primer

let rec canBeFormed goldbach prime x = 
     if(prime < 1.0) then false
     else if(isPrime(prime) = false) then canBeFormed goldbach (prime - 2.0) x
     else 
        match prime + 2.0 * (x * x) with
        | a when a = goldbach -> true
        | a when a > goldbach -> canBeFormed goldbach (prime - 2.0) 1.0
        | _ -> canBeFormed goldbach prime (x+1.0)
 
let rec findGoldbach x result = 
    if(result > 0.0) then result
    else if(isPrime(x)) then findGoldbach (x + 2.0) result
    else 
        let resu = canBeFormed x (x - 2.0) 1.0
        if(resu = false) then findGoldbach x x
        else
            findGoldbach (x + 2.0) result

let result = findGoldbach 3.0 0.0