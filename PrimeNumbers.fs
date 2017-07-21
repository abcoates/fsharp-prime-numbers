module org.contakt.math.prime.PrimeNumbers

/// <summary>Given a value 'n', a prime number, and a multiple of that prime, increase the multiple until it is >= n then return it.</summary>
/// <param name="n">Value which sets a new minimum bound for the prime multiple.</param>
/// <param name="prime">A prime number.</param>
/// <param name="primemult">A multiple of the prime number.</param>
/// <returns>A new prime multiple that is >= n.</returns>
let rec incprime (n: int) (prime: int) (primemult: int): int =
    if (primemult >= n)
    then primemult
    else incprime n prime (primemult + prime)

/// <summary>Given a value 'n' and a pair of a prime number and a multiple of that prime, returns whether n is a multiple of that prime
/// along with a new prime/multiple pair where the multiple is >= n.</summary>
/// <param name="n">Value which sets a new minimum bound for the prime multiple.</param>
/// <param name="primepair">A tuple of a prime number and a multiple of that prime number.</param>
/// <param name="primemult">A multiple of the prime number.</param>
/// <returns>A tuple of a boolean and a new prime pair.</returns>
let factortest (n:int) (primepair: int*int): bool*(int*int) =
    let prime = fst primepair
    let primemult = snd primepair
    if (primemult > n) 
    then (false, (prime, primemult))
    else
        if (primemult = n)
        then (true, (prime, primemult))
        else
            let newmult = incprime n prime primemult
            (newmult = n, (prime, newmult))

/// <summary>Given a value 'n' and a set of pairs of primes and prime multiples for all primes < n.</summary>
/// <param name="result">.</param>
/// <param name="n">.</param>
/// <returns>.</returns>
let calculateprimes (result: (int*int) list) (n: int): (int*int) list =
    let testprimes = result |> List.map (factortest n)
    let notPrime: bool = testprimes |> List.map fst |> List.exists id
    if (notPrime)
    then testprimes |> List.map snd
    else (List.append (testprimes |> List.map snd) [(n,n)])

/// <summary>Initial prime/multiple pair.</summary>
let initPrimes = [(2,2)]

/// <summary>Returns the primes that are <= 'nmax'.</summary>
/// <param name="nmax">Value up to which primes are calculated.</param>
/// <returns>List of prime numbers <= 'nmax'.</returns>
let primesUpTo (nmax: int): int list =
    match nmax with
    | 2 -> [2]
    | n when n < 2 -> []
    | _ -> [3..nmax] |> List.fold calculateprimes initPrimes |> List.map fst

/// <summary>Iteratively applies 'f' to 'x', then to 'f x', etc. until the result causes the predicate 'p' to return 'true'.</summary>
/// <param name="f">Function to apply iteratively.</param>
/// <param name="p">Predicate function that returns 'true' when the iteration should stop.  Its parameters are the previous state and the current state.</param>
/// <param name="x">Starting value to which 'f' is applied iteratively.</param>
let rec iterateUntil (f: 'a -> 'a) (p: 'a -> 'a -> bool) (x: 'a): 'a =
    let y = f x
    if (p x y)
    then y
    else iterateUntil f p y

let calculateNextPrime (result: (int*int) list): (int*int) list =
    let nmin = fst (result |> List.last) + 1
    let p (result: 'a list) (newresult: 'a list) = (List.length newresult) > (List.length result)
    let p2 (result: 'b*('a list)) (newresult: 'b*('a list)) = (List.length (snd newresult)) > (List.length (snd result))
    let rec f state =
        let n = fst state
        let result = snd state
        let newresult = calculateprimes result n
        if (p result newresult)
        then (n, newresult)
        else f (n+1, newresult)
    snd (iterateUntil f p2 (nmin, result))

/// <summary>Returns the (infinite) sequence: x, f x, f (f x), etc.</summary>
let rec iterate (f: 'a -> 'a) (x: 'a): 'a seq =
    seq {
        yield x
        yield! (iterate f (f x))
    }

/// <summary>Returns the (infinite) transformed sequence: t x, t (f x), t (f (f x)), etc.</summary>
let rec iterateAndTransform (t: 'a -> 'b) (f: 'a -> 'a) (x: 'a): 'b seq =
    seq {
        yield (t x)
        yield! (iterateAndTransform  t f (f x))
    }

/// <summary>Infinite iterator over all of the prime numbers.</summary>
let primes: int seq = iterateAndTransform (fun x -> fst (List.last x)) calculateNextPrime initPrimes

/// <summary>Returns the maximum power of 'p' that is exactly divisible into 'n'.</summary>
/// <param name="n">Value into which 'p' is divided.</param>
/// <param name="p">Value to divide into 'n'.</param>
/// <returns>The maximum power of 'p' that is exactly divisible into 'n'.</returns>
let rec maxMultiple (n: int) (p: int): int =
    if ((n = 0) || (p = 0))
    then 0
    else
        let rem = n % p
        match rem with
        | 0 -> 1 + (maxMultiple (n / p) p)
        | _ -> 0

/// <summary>Returns a factorisation of 'n' in terms of the given sequence of factors.
/// Note: will not terminate if 'n' can't be factorised by the factors in the sequence.</summary>
/// <param name="n">Value to be factorised.</param>
/// <param name="factors">Sequence of factors to use for factorisation.</param>
/// <returns>Sequence of (factor,power) pairs.</returns>
let rec factorise (n: int) (factors: int seq): (int*int) seq =
    seq {
        match n with
        | 0 | 1 -> yield! Seq.empty
        | neg when neg < 0 -> yield! (factorise (-n) factors)
        | _ ->
            let factor = Seq.head factors
            let power = maxMultiple n factor
            if (power >= 1) then
                yield (factor,power)
                yield! (factorise (n/(pown factor power)) (Seq.tail factors))
            else
                yield! (factorise n (Seq.tail factors))
    }

/// <summary>Returns a factorization of 'n' in terms of the given sequence of factors.
/// Note: will not terminate if 'n' can't be factorized by the factors in the sequence.</summary>
/// <param name="n">Value to be factorized.</param>
/// <param name="factors">Sequence of factors to use for factorization.</param>
/// <returns>Sequence of (factor,power) pairs.</returns>
let factorize (n: int) (factors: int seq): (int*int) seq = factorise n factors

/// <summary>Returns the prime factors of 'n'.</summary>
/// <param name="n">Value to be factorised/factorized.</param>
/// <returns>List of (factor,power) pairs.</returns>
let rec primeFactorsOf (n: int): (int*int) list =
    match n with
    | 0 -> []
    | 1 -> [(1,1)]
    | neg when neg < 0 -> primeFactorsOf (-neg)
    | _ -> factorise n primes |> Seq.toList
