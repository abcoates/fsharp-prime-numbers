#r "bin\Release\PrimeNumbers.dll"
open org.contakt.math.prime.PrimeNumbers
open System
let sw = System.Diagnostics.Stopwatch()
let rec printEvery (n: int) (index: int) (s: int seq) =
    let newindex = index + n
    printfn "%A" (newindex, s |> Seq.take n |> Seq.last, (sw.ElapsedMilliseconds + 500L) / 1000L)
    printEvery n newindex (s |> Seq.skip n)
printfn "(#, prime, sec)"
sw.Start()
printEvery 1000 0 primes
