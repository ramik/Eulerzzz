module Problem206

open System
open System.Numerics
open System.Text.RegularExpressions

let start = 1414213562I
let pattern = "1.2.3.4.5.6.7.8.9.0"

let rec findSolution x = 
    if(x % 10000000I = 0I) then printfn "%A" x
    let candidate = BigInteger.Pow(x, 2)
    match Regex.Match(candidate.ToString(), pattern) with
    | m when m.Success -> x
    | _ -> findSolution (x - 1I)