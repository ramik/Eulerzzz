module Problem50

#if INTERACTIVE
#r @"..\lib\xunit.dll"
#load "Primer.fs"
#endif

open System
open Primer

let checkCandidate cand skipValue  = 
    let rec checkCandidateF sum count result primes = 
        let head = Seq.head primes
        match sum + head with 
        | x when cand < x -> result
        | x when isPrime x -> checkCandidateF x (count + 1.0) count (Seq.skip 1 primes)
        | x -> checkCandidateF x (count + 1.0) result (Seq.skip 1 primes)
    checkCandidateF 0.0 1.0 0.0 (Seq.skip skipValue Primes)

let t y howManyToSkip = 
    async { 
         return (howManyToSkip, checkCandidate y howManyToSkip) 
    }

let findSolution x = 
    let tasks = [ for i in 0..10 -> t x i ]
    Async.Parallel tasks 
        |> Async.RunSynchronously
        |> Seq.map (fun c -> c) 
        |> Seq.maxBy (fun (_, c) -> c)
        |> (fun (c, d) -> Primes |> Seq.skip c |> Seq.take (Convert.ToInt32 d))
        |> Seq.sum
