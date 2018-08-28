// **** Method #2 is usefully & increasingly faster than method #1 - but using bigint is a big slowdown. ****

module org.contakt.math.prime.PrimeNumbers2

type Prime = int // bigint
type PrimePair = Prime * Prime
type PrimeList = PrimePair list
type PrimeState = PrimeList

let int2Prime (n:int) = n // bigint n

// At each iteration, check if the next number matching the 2nd of any pair.
// If so, it is not a prime, increment the 2nd of that pair and continue through the pairs.
// If not, it is a prime -continue through the pairs, but then add the value and its double as a new pair,
// and yield the value as the next prime in the sequence.

let PrimeStateInit: PrimeState = [(int2Prime 2, int2Prime 4)] // first prime and its double

type PrimeCheckAccumulator = Prime * bool * PrimeState
let private primeCheck (value:Prime) (state:PrimeState): bool * PrimeState =
    let accumInit: PrimeCheckAccumulator = (value, true, [])
    let onePrimeCheck (accum: PrimeCheckAccumulator) (pair:PrimePair): PrimeCheckAccumulator =
        let value, result, pairs = accum
        if (value = (snd pair))
        then (value, false, (fst pair, (snd pair) + (fst pair)) :: pairs)
        else (value, result, pair :: pairs)
    let _, isPrime, newPrimeState = (state |> List.rev |> List.fold onePrimeCheck accumInit)
    (isPrime, newPrimeState)

let rec private nextPrime (value:Prime) (state:PrimeState): Prime * PrimeState =
    let isPrime, newPrimeState = primeCheck value state
    if (isPrime)
    then (value, (value, value * (int2Prime 2))::newPrimeState)
    else nextPrime (value + (int2Prime 1)) newPrimeState

let private primesMaster = seq<Prime> {
    yield (int2Prime 2)
    yield! Seq.unfold (fun (state:PrimeState) ->
        let n = (int2Prime 1) + (state |> List.head |> fst)
        Some( nextPrime n state )
    ) PrimeStateInit
}

/// <summary>Infinite iterator over all of the prime numbers.</summary>
let primes = Seq.cache primesMaster
