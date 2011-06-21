module problem1
#if INTERACTIVE
#r @"C:\Eulerzzz\lib\xunit.dll"
#endif

let initSequence = Seq.initInfinite id |> Seq.filter (fun x -> x % 3 = 0 || x % 5 = 0)
let takeItems max = Seq.takeWhile (fun x -> x < max) initSequence

module problem1UnitTests = 
  open Xunit

  [<Fact>]
  let sumsUpCorrectlyWhenBelow10() = 
    let sum = takeItems 10 |> Seq.sum
    Assert.Equal(23, sum)

  [<Fact>]
  let problemSolution() =
    let sum = takeItems 1000 |> Seq.sum
    Assert.Equal(233168, sum)

module printSolution =
  let sum = takeItems 1000 |> Seq.sum
  printfn "Problem1 solution is: %i" sum