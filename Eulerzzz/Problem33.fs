module problem33
#if INTERACTIVE
#r @"C:\Eulerzzz\lib\xunit.dll"
#endif

open System

let wouldResultZeroAfterSimplification x = match x with | (a, b) when a % 10 = 0 || b % 10 = 0 -> true
                                                        | _ -> false  
                                                           
let toReverseInt x = 
    let toReverseIntInternal (y : string) = y.ToCharArray() |> Array.rev |> Array.fold (fun acc a -> String.Format("{0}{1}", acc, a)) ""
    Int32.Parse(toReverseIntInternal (x.ToString()))

let isPalindrome x = match x with | (a, b) when a = toReverseInt b -> true  
                                  | _ -> false
      
module problem1UnitTests = 

  open Xunit

  let trivialityCheckingOnTrivials =
    let candidates = [(10, 20);(70, 80);(17, 70);(70, 97)]
    Assert.Equal(4, candidates |> Seq.filter wouldResultZeroAfterSimplification |> Seq.length)

  let trivialityCheckingOnNonTrivials =
    let candidates = [(11, 22);(57, 75);(21, 44);(56, 65)]
    Assert.Equal(0, candidates |> Seq.filter wouldResultZeroAfterSimplification |> Seq.length)

  let intReversing =
    let candidates = [98; 11; 90; 75]
    Assert.Equal([89; 11; 9; 57], (candidates |> List.map toReverseInt))

  let isPalindromeCheckOnPalindromes =
    let candidates = [(89, 98); (11, 11); (57, 75)]
    Assert.Equal(3, Seq.filter isPalindrome candidates |> Seq.length)

  let isPalindromeCheckOnNonPalindromes =
    let candidates = [(89, 99); (11, 12); (55, 75)]
    Assert.Equal(0, Seq.filter isPalindrome candidates |> Seq.length)

  [<Fact>] 
  let trivialityCheckingOnTrivialsGUI() = trivialityCheckingOnTrivials

  [<Fact>] 
  let trivialityCheckingOnNonTrivialsGUI() = trivialityCheckingOnNonTrivials

  [<Fact>] 
  let intReversingGUI() = intReversing

module printSolution =
  printfn "%s" "not done"