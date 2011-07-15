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
                              match a, b, c, d with | _ when a = b -> (c, d) 
                                                    | _ when c = d-> (a, b)
                                                    | _ -> simplifyInternal (toReverseInt first) second
                 simplifyInternal (fst x) (snd x)

let areSame a b = (float (fst a)) / (float (snd a)) = (float (fst b)) / (float (snd b))

let generateFractions max =
    let rec generateFractionsInternal x y result =
        match x, y with | _ when y = max -> result 
                        | _ when x = y -> generateFractionsInternal 10 (y + 1) result
                        | _ -> generateFractionsInternal (x + 1) y (Seq.append result [(x,y)])
    generateFractionsInternal 10 11 Seq.empty

let findSolution = let fractions = generateFractions 100
                   let simplifiableFractions = fractions
                                               |> Seq.filter (fun c -> (wouldResultZeroAfterSimplification c = false)) 
                                               |> Seq.filter (fun c -> (isPalindrome c) = false)
                                               |> Seq.filter canBeSimplified
                   let resultFractions = simplifiableFractions |> Seq.map simplify |> Seq.zip simplifiableFractions
                                                               |> Seq.filter (fun c -> areSame (fst c) (snd c))
                                                               |> Seq.map (fun c -> (snd c))
                   let nominator = resultFractions |> Seq.fold (fun acc c -> acc * (fst c)) 1 
                   let denominator = resultFractions |> Seq.fold (fun acc c -> acc * (snd c)) 1 
                   (nominator, denominator)                   

module problem33Unitests = 
  open Xunit

  let generateFractionsDoesFractionSeq =
      let candidates = generateFractions 14
      Assert.Equal ([(10, 11); (10, 12); (11,12);(10,13);(11,13);(12,13)], Seq.toList candidates)

  let areSameReturnsTrueWhenSame =
      let candidates = List.toSeq [((1,2), (2,4));((4,8),(49,98))]
      Assert.True (Seq.forall (fun x -> areSame (fst x) (snd x)) candidates)

  let areSameReturnsFalseWhenNotSame =
      let candidates = List.toSeq [((1,2), (2,3));((3,8),(49,98))]
      Assert.True (Seq.forall (fun x -> ((areSame (fst x) (snd x)) = false)) candidates)

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

module printSolution = printfn "Solution's result is %A" findSolution