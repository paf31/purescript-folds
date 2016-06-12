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
data Fold a b
```

##### Instances
``` purescript
Profunctor Fold
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

#### `unfoldFold`

``` purescript
unfoldFold :: forall s a b. s -> (s -> a -> s) -> (s -> b) -> Fold a b
```

#### `unfoldFold_`

``` purescript
unfoldFold_ :: forall a b. b -> (b -> a -> b) -> Fold a b
```

#### `foldl`

``` purescript
foldl :: forall f a b. Foldable f => Fold a b -> f a -> b
```

#### `scanl`

``` purescript
scanl :: forall f a b. Traversable f => Fold a b -> f a -> f b
```

#### `mconcat`

``` purescript
mconcat :: forall m. Monoid m => Fold m m
```

#### `head`

``` purescript
head :: forall a. Fold a (Maybe a)
```

#### `last`

``` purescript
last :: forall a. Fold a (Maybe a)
```

#### `null`

``` purescript
null :: forall a. Fold a Boolean
```

#### `length`

``` purescript
length :: forall a s. Semiring s => Fold a s
```

#### `and`

``` purescript
and :: forall b. HeytingAlgebra b => Fold b b
```

#### `or`

``` purescript
or :: forall b. HeytingAlgebra b => Fold b b
```

#### `all`

``` purescript
all :: forall a b. HeytingAlgebra b => (a -> b) -> Fold a b
```

#### `any`

``` purescript
any :: forall a b. HeytingAlgebra b => (a -> b) -> Fold a b
```

#### `sum`

``` purescript
sum :: forall s. Semiring s => Fold s s
```

#### `product`

``` purescript
product :: forall s. Semiring s => Fold s s
```

#### `maximum`

``` purescript
maximum :: forall a. Bounded a => Fold a a
```

#### `minimum`

``` purescript
minimum :: forall a. Bounded a => Fold a a
```

#### `elem`

``` purescript
elem :: forall a. Eq a => a -> Fold a Boolean
```

#### `notElem`

``` purescript
notElem :: forall a. Eq a => a -> Fold a Boolean
```


