module problem34
#if INTERACTIVE
#r @"C:\Eulerzzz\lib\xunit.dll"
#endif

open System

let factorial x = 
  let rec factorialInternal x y = 
    match x with | 0 -> 1
                 | 1 -> y
                 | _ -> factorialInternal (x-1) y*x
  factorialInternal x 1

let calculateFactorialSums x = x.ToString().ToCharArray() |> Seq.map (fun c -> Int32.Parse(c.ToString())) |> Seq.map (fun c -> factorial(c))
                                                          |> Seq.sum 

let findSolution x =
  let rec findSolutionInternal x result =
    match x with | 1000000 -> result
                 | _ when x = calculateFactorialSums x -> findSolutionInternal (x + 1) (result + x)
                 | _ -> findSolutionInternal (x + 1) result
  findSolutionInternal x 0

module problem34Unitests = 
  open Xunit

  let factorialCalculationSpec = Assert.Equal([1;1;2;6;24;120;720], [0;1;2;3;4;5;6] |> List.map (fun c -> factorial(c)))
  let calculateFactorialSumsSpec = Assert.Equal([3;3;8;145;5762], [100;101;103;145;267] |> List.map (fun c -> calculateFactorialSums(c)))

module printSolution =
  printfn "Solution's result is %i" (findSolution 3)