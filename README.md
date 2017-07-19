# fsharp-prime-numbers
### F# module for generating prime numbers

This is a small library for generating prime numbers.
It doesn't follow any particularly well-known algorithm,
but you might consider it to be a variation on the Sieve of Eratosthenes.

The F# module is 'org.contakt.math.prime.PrimeNumbers'.
The key functions that it exports are:

1. primesUpTo (nmax: int): int list - returns a list of the primes <= 'nmax'
1. primes: int seq - returns an infinite sequence of the prime numbers (bounded in practice, though, by the amount of memory available)

Look at 'prime-number-examples.fsx' for examples of how to use the key functions.
