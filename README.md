Limited equirecursive types for Haskell.

Haskell really doesn't support equirecursive types, so we cheat a bit.

This package has three main parts:
- A type checker for monomorphic equirecursive types, using type families
- A number of methods for testing type families
- A nice collection of example and practical modules that require equirecursive types

So, how do we cheat? Consider an implementation of `repeat 0` using tuples:
```
λ> let zeroes = let x = (0, x) in x
```
Functionally, thanks to Haskell's laziness, this won't loop forever unless you 
try to access the whole thing at once. However, a problem arises when you look
at it's type 
```
zeroes :: Num a => (a, (a, (a, (a, ...

GHCI: Occurs check: cannot construct the infinite type: t3 ~ (t2, t3)
```

There's a way around this, by using `unsafeCoerce` and `Any`:
```
λ> let zeroesGenerator = (unsafeCoerce (\x -> (0, x)) :: (Int, Any) -> (Int, Any))
zeroesGenerator ∷ (Int, Any) → (Int, Any)

λ> let zeroes = fix zeroesGenerator
zeroes ∷ (Int, Any)

λ> fst zeroes
0

λ> fst (unsafeCoerce (snd zeroes) :: (Int, Any))
0
```

It actually works! Thanks to Haskell's laziness, `zeroes` is represented as `(Int, thunk)`,
_and_ we can step forward a single step in the thunk's evaluation to get: `(Int, (Int, thunk))`.


