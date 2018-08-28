// **** Method #2 is usefully & increasingly faster than method #1 - but using bigint is a big slowdown. ****

#r "bin\Release\PrimeNumbers.dll"
open org.contakt.math.prime.PrimeNumbers2

let last aList = aList |> List.rev |> List.head

#time
let primes5 = primes |> Seq.take 5 |> Seq.toList
let prime5 = last primes5

let primes20 = primes |> Seq.take 20 |> Seq.toList
let prime20 = last primes20

let primes50 = primes |> Seq.take 50 |> Seq.toList
let prime50 = last primes50

let primes100 = primes |> Seq.take 100 |> Seq.toList
let prime100 = last primes100

let primes1k = primes |> Seq.take 1000 |> Seq.toList
let prime1k = last primes1k

let primes10k = primes |> Seq.take 10000 |> Seq.toList
let prime10k = last primes10k

let primes100k = primes |> Seq.take 100000 |> Seq.toList
let prime100k = last primes100k
