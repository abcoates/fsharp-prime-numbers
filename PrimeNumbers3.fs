// **** Method #3 is easily the fastest - using bigint is a big slowdown, but not so onerous. ****

module org.contakt.math.prime.PrimeNumbers3

type Prime = int // bigint
type PrimeMap = Map<Prime, Prime list>
type PrimeState = PrimeMap

let int2Prime (n:int) = n // bigint n

let PrimeStateInit: PrimeState = [(int2Prime 4, [int2Prime 2])] |> Map.ofList // first prime and its double

let rec private updateState (value:Prime) (primes:Prime list) (state:PrimeState): PrimeState =
    match primes with
    | [] -> state
    | _ ->
        let nextPrime = List.head primes
        let newKey = value + nextPrime
        if (state.ContainsKey(newKey))
        then
            updateState value (List.tail primes) (state.Add(newKey, nextPrime::(state.[newKey])))
        else
            updateState value (List.tail primes) (state.Add(newKey, [nextPrime]))

let rec private nextPrime (value:Prime) (state:PrimeState): Prime * PrimeState =
    if (state.ContainsKey(value))
    then
        let valuePrimes = state.[value]
        let newState = updateState value valuePrimes (state.Remove(value))
        nextPrime (value + (int2Prime 1)) newState
    else
        (value, state.Add(value * (int2Prime 2), [value]))

let private primesMaster = seq<Prime> {
    yield (int2Prime 2)
    yield! Seq.unfold (fun (state:PrimeState) ->
        let n = (int2Prime 1) + (state |> Map.toList |> List.map snd |> List.concat |> List.max)
        Some( nextPrime n state )
    ) PrimeStateInit
}

/// <summary>Infinite iterator over all of the prime numbers.</summary>
let primes = Seq.cache primesMaster
