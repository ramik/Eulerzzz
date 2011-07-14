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

let canBeSimplified x = let a =  Array.toSeq ((fst x).ToString().ToCharArray())
                        let b =  Array.toSeq ((snd x).ToString().ToCharArray())
                        Seq.exists (fun c -> Seq.exists (fun t -> c=t) b) a

let simplify x = let rec simplifyInternal first second = 
                              let a = first % 10
                              let b = second % 10
                              let c = first / 10
                              let d = second / 10
                              printfn "%i %i %i %i" a b c d
                              match a, b, c, d with | _ when a = b -> (c, d) 
                                                    | _ when c = d-> (a, b)
                                                    | _ -> simplifyInternal (toReverseInt first) second
                 simplifyInternal (fst x) (snd x)
      
module problem33Unitests = 
  open Xunit

  let simplificationOnSimpliables =
      let toSimplify = [(14, 24);(47, 94);(19, 94); (81, 82)]
      Assert.Equal ([(1,2);(7,9);(1,4);(1,2)], List.map simplify toSimplify)

  let canBeSimplifiedCheckingOnSimplifiable =
    let candidates = [(11, 21);(76, 96);(17, 74);(49, 98)]
    Assert.Equal(4, candidates |> Seq.filter canBeSimplified |> Seq.length)
  
  let canBeSimplifiedCheckingOnNNonSimplifiable =
    let candidates = [(11, 22);(76, 99);(17, 84);(58, 97)]
    Assert.Equal(0, candidates |> Seq.filter canBeSimplified |> Seq.length)

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

module printSolution =
  printfn "%s" "not done"