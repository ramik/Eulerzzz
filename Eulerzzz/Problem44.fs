module Problem44

open System

let pentagonal x = (x * (3 * x - 1)) / 2

let isPentagonal x =
    let rec isPentagonalValue candidate = 
        match pentagonal candidate with
        | head when x = head -> true
        | head when head > x -> false
        | _ -> isPentagonalValue (candidate + 1)
    isPentagonalValue 2

let getPentagonalPairs =
    let howMany = 2500
    let rec getPairs x y result = 
        if(x > howMany) then result
        else if(y > howMany) then getPairs (x + 1) (x + 2) result
        else 
            let xvalue = pentagonal x
            let yvalue = pentagonal y
            if(isPentagonal(yvalue - xvalue) && isPentagonal(xvalue + yvalue)) then 
                getPairs x (y + 1) ((xvalue, yvalue) :: result)
            else getPairs x (y + 1) result 
    getPairs 2 3 []

let result = getPentagonalPairs |> Seq.map (fun (first, second) -> second - first) |> Seq.min