module Problem45

let triangle x = (x * (x + 1L)) / 2L
let pentagonal x = (x * (3L * x - 1L)) / 2L
let hexagonal x = x * (2L * x - 1L)

let rec pairings x y z =
    let hexa = hexagonal x
    let penta = pentagonal y
    let tri = triangle z
    match hexa, penta, tri with 
    | a, b, c when a = b && b = c -> a
    | a, b, c when a < b || b < c -> pairings (x+1L) (x+1L) (x+1L)
    | a, b, c when a = b && b > c -> pairings x y (z+1L)
    | _ -> pairings x (y+1L) z

let result = pairings 144L 144L 144L