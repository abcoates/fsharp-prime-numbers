#r "bin\Release\PrimeNumbers.dll"
open org.contakt.math.prime.PrimeNumbers
open System
let sw = System.Diagnostics.Stopwatch()
let mutable n: uint32 = 0u
sw.Start()
n <- 0u; primes |> Seq.iter (fun i -> n <- n + 1u; if (n % 1000u = 0u) then printfn "prime #%d = %d in %d seconds" n i ((sw.ElapsedMilliseconds + 500L) / 1000L))
