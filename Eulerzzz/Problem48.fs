module Problem48

open System
open System.Numerics

let findResult howmany = 
    let rec findResultF number exp result = 
        if(exp > howmany) then result
        else
            let power = BigInteger.Pow(number, exp)
            findResultF (number + 1I) (exp + 1) (result + power)
    
    findResultF 1I 1 0I

let findResultStr = findResult 1000 |> (fun c -> c.ToString())
let result = 
    let resultStr = findResultStr.ToCharArray()
    let len = resultStr |> Seq.length
    resultStr |> Seq.skip (len - 10) |> (fun c -> String.Join("", c))
                