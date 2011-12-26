module Problem39

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

let getSidesForTriangle total = 
  let rec calc a b c result = 
        let endcond = a > total / 3 
        match endcond, b >= c with
          | (true, _) -> result
          | (_, true) -> calc (a + 1) (a + 2) (total - a - a - 3) result
          | _ -> calc a (b + 1) (c - 1) ((a,b,c) :: result)
                     
  calc 1 2 (total - 3) []

let isValidSideCombo (a,b,c) = pown a 2 + pown b 2 = pown c 2
let a = isValidSideCombo (20, 30, 40)

module problem39Unitests = 
  open Xunit

  let getSidesForTriangle10and12 = 
    Assert.Equal([(3, 4, 5); (2, 4, 6); (2, 3, 7); (1, 5, 6); (1, 4, 7); (1, 3, 8); (1, 2, 9)], getSidesForTriangle 12)
    Assert.Equal([(2, 3, 5); (1, 4, 5); (1, 3, 6); (1, 2, 7)], getSidesForTriangle 10)
  
  let isValidComboOnValidCombos = 
    Assert.True(isValidSideCombo(30, 40, 50))
    Assert.True(isValidSideCombo(24, 45, 51))  

  let isValidComboOnInValidCombos = 
    Assert.False(isValidSideCombo(31, 39, 50))
    Assert.False(isValidSideCombo(22, 47, 51))  