module Problem43

open System 

let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let divisors = [2;3;5;7;11;13;17]
let hasProperty = 
    Seq.skip 1
    >> Seq.map (fun c -> c.ToString()) 
    >> Seq.windowed 3 
    >> Seq.map (fun c -> String.Join("", c))
    >> Seq.map Convert.ToInt32
    >> Seq.zip divisors
    >> Seq.forall (fun (divisor, dividend) -> dividend % divisor = 0)

let result =
    permutations [1;2;3;4;5;6;7;8;9;0] 
    |> Seq.filter hasProperty 
    |> Seq.map (fun c -> Seq.map (fun g -> g.ToString()) c)   
    |> Seq.map (fun c -> String.Join("", c))  
    |> Seq.map Convert.ToInt64
    |> Seq.sum
   