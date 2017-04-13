## Module Control.Fold

This module provides a type `Fold` for lefts folds, which can be combined
using the `Applicative` class:

```purescript
average :: Fold Number Number
average = (/) <$> sum <*> length
```

`Fold` can be used to fold a `Foldable` structure (`foldl`), or scan a
`Traversable` structure (`scanl`):

```purescript
finalAverage = foldl average [1.0, 2.0, 3.0] :: Number
movingAverage = scanl average [1.0, 2.0, 3.0] :: Array Number
```

This library is based on the `foldl` library by Gabriel Gonzalez:
<http://hackage.haskell.org/package/foldl>

#### `Fold`

``` purescript
newtype Fold a b
```

A left fold, which takes zero or more values of type `a` as input
and produces output of type `b`.

##### Instances
``` purescript
Profunctor Fold
Closed Fold
Functor (Fold a)
Apply (Fold a)
Applicative (Fold a)
Extend (Fold a)
Comonad (Fold a)
(Semigroup b) => Semigroup (Fold a b)
(Monoid b) => Monoid (Fold a b)
```

#### `stepFold`

``` purescript
stepFold :: forall a b. a -> Fold a b -> Fold a b
```

Step a fold by providing a single input.

#### `unfoldFold`

``` purescript
unfoldFold :: forall s a b. s -> (s -> a -> s) -> (s -> b) -> Fold a b
```

Create a `Fold` by providing an initial state, a function which updates
that state, and a function which produces output from a state.

#### `unfoldFold_`

``` purescript
unfoldFold_ :: forall a b. b -> (b -> a -> b) -> Fold a b
```

Create a `Fold` by providing an initial state and a function which updates
that state. This is a variant of `unfoldFold` where the output is the state
itself.

#### `foldl`

``` purescript
foldl :: forall f a b. Foldable f => Fold a b -> f a -> b
```

Run a `Fold` by providing a `Foldable` container of inputs, and then
generating a single output. This is analogous to the `foldl` function from
`Data.Foldable`.

#### `scanl`

``` purescript
scanl :: forall f a b. Traversable f => Fold a b -> f a -> f b
```

Run a `Fold` by providing a `Traversable` container of inputs, and
generating an output for each input. This is analogous to the `scanl` function from
`Data.Traversable`.

#### `distributed`

``` purescript
distributed :: forall f a b. Distributive f => Fold a b -> Fold (f a) (f b)
```

Fold over entire collections of inputs, producing a collection of outputs.

#### `mconcat`

``` purescript
mconcat :: forall m. Monoid m => Fold m m
```

`Fold` values in some `Monoid`.

#### `head`

``` purescript
head :: forall a. Fold a (Maybe a)
```

A `Fold` which remembers the first input.

#### `last`

``` purescript
last :: forall a. Fold a (Maybe a)
```

A `Fold` which keeps the last input.

#### `null`

``` purescript
null :: forall a. Fold a Boolean
```

A `Fold` which tests whether any inputs were seen.

#### `length`

``` purescript
length :: forall a s. Semiring s => Fold a s
```

A `Fold` which counts its inputs.

#### `and`

``` purescript
and :: forall b. HeytingAlgebra b => Fold b b
```

A `Fold` which tests if _all_ of its inputs were true
(generalized to work with an arbitrary `HeytingAlgebra`).

#### `or`

``` purescript
or :: forall b. HeytingAlgebra b => Fold b b
```

A `Fold` which tests if _any_ of its inputs were true
(generalized to work with an arbitrary `HeytingAlgebra`).

#### `all`

``` purescript
all :: forall a b. HeytingAlgebra b => (a -> b) -> Fold a b
```

A `Fold` which tests if _all_ of its inputs satisfy some predicate
(generalized to work with an arbitrary `HeytingAlgebra`).

#### `any`

``` purescript
any :: forall a b. HeytingAlgebra b => (a -> b) -> Fold a b
```

A `Fold` which tests if _any_ of its inputs satisfy some predicate
(generalized to work with an arbitrary `HeytingAlgebra`).

#### `sum`

``` purescript
sum :: forall s. Semiring s => Fold s s
```

A `Fold` which computes the sum of its inputs
(generalized to work with an arbitrary `Semiring`).

#### `product`

``` purescript
product :: forall s. Semiring s => Fold s s
```

A `Fold` which computes the product of its inputs
(generalized to work with an arbitrary `Semiring`).

#### `maximum`

``` purescript
maximum :: forall a. Bounded a => Fold a a
```

A `Fold` which computes the maximum of its inputs
(generalized to work with an arbitrary `Bounded` type).

#### `minimum`

``` purescript
minimum :: forall a. Bounded a => Fold a a
```

A `Fold` which computes the minimum of its inputs
(generalized to work with an arbitrary `Bounded` type).

#### `elem`

``` purescript
elem :: forall a. Eq a => a -> Fold a Boolean
```

A `Fold` which tests if a specific value appeared as an input.

#### `notElem`

``` purescript
notElem :: forall a. Eq a => a -> Fold a Boolean
```

A `Fold` which tests if a specific value did not appear as an input.


