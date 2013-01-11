module Problem42

open System 
open System.IO

let wordsTxt = File.ReadAllText("c:/projects/Eulerzzz/Eulerzzz/words.txt")

let countTriangle (n:int) = Convert.ToInt32( 0.5 * Convert.ToDouble(n) * (Convert.ToDouble(n) + 1.0) ) 

let wordValue (x: string) = x.ToCharArray() |> Array.toSeq |> Seq.map (fun c -> Convert.ToInt32(c) - 64) |> Seq.sum

let triangles = Seq.unfold (fun state -> Some(countTriangle(state), state + 1)) 1 |> Seq.take 40
let isTriangle x = triangles |> Seq.exists (fun c -> c = x) 

let words = wordsTxt.Split(',') |> Array.toSeq 
                |> Seq.map (fun x -> x.Replace('"', ' ').Trim())
                |> Seq.map wordValue 
                |> Seq.filter isTriangle 
                |> Seq.length
              

