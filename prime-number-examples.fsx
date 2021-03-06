﻿// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "bin\Release\PrimeNumbers.dll"
open org.contakt.math.prime.PrimeNumbers

// How to get the prime numbers up to a given value.
let nmax = 100
printfn "#### 1 #### Prime numbers up to %d ####" nmax
let result1 = primesUpTo nmax
printfn "%A" result1

// How to get an (infinite) sequence of prime numbers
printfn "\n#### 2 #### Infinite sequence of prime numbers ####"
printfn "%A" primes

// How to get the first N prime numbers
let N = 20
printfn "\n#### 3 #### First %d prime numbers ####" N
let result2 = primes |> Seq.take N |> Seq.toList
printfn "%A" result2

// How to get the prime factors of an integer
let n = 14400
printfn "\n#### 4 #### Prime factors of %d ####" n
let result3 = primeFactorsOf n
printfn "%A" result3
let n2 = result3 |> List.map (fun pair -> pown (fst pair) (snd pair)) |> List.fold (*) 1
printfn "Product of factors is: %d" n2
