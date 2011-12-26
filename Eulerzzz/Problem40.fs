module Problem40

open System

let a = Seq.initInfinite (fun c -> c + 1) |> Seq.takeWhile (fun c -> c < 200000) |> Seq.map (fun c -> c.ToString()) |> String.concat ""
let b = a.ToCharArray()
let nthItems = [0;9;99;999;9999;99999;999999]
let result = nthItems |> Seq.map (fun c -> Seq.nth c b) |> Seq.map (fun c -> Int32.Parse (c.ToString())) |> Seq.fold (*) 1

