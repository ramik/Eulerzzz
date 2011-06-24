module problem33

let wouldResultZeroAfterSimplification x = match x with | (a, b) when a % 10 = 0 || b % 10 = 0 -> true
                                                        | _ -> false           
module problem1UnitTests = 
  #if INTERACTIVE
  #r @"C:\Eulerzzz\lib\xunit.dll"
  #endif
  open Xunit

  let trivialityCheckingOnTrivials =
    let candidates = [(10, 20);(70, 80);(17, 70);(70, 97)]
    Assert.Equal(4, candidates |> Seq.filter wouldResultZeroAfterSimplification |> Seq.length)

  let trivialityCheckingOnNonTrivials =
    let candidates = [(11, 22);(57, 75);(21, 44);(56, 65)]
    Assert.Equal(0, candidates |> Seq.filter wouldResultZeroAfterSimplification |> Seq.length)

  [<Fact>] 
  let trivialityCheckingOnTrivialsGUI() = trivialityCheckingOnTrivials

  [<Fact>] 
  let trivialityCheckingOnNonTrivialsGUI() = trivialityCheckingOnNonTrivials


module printSolution =
  printfn "%s" "not done"