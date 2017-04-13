-- | This module provides a type `Fold` for lefts folds, which can be combined
-- | using the `Applicative` class:
-- |
-- | ```purescript
-- | average :: Fold Number Number
-- | average = (/) <$> sum <*> length
-- | ```
-- |
-- | `Fold` can be used to fold a `Foldable` structure (`foldl`), or scan a
-- | `Traversable` structure (`scanl`):
-- |
-- | ```purescript
-- | finalAverage = foldl average [1.0, 2.0, 3.0] :: Number
-- | movingAverage = scanl average [1.0, 2.0, 3.0] :: Array Number
-- | ```
-- |
-- | This library is based on the `foldl` library by Gabriel Gonzalez:
-- | <http://hackage.haskell.org/package/foldl>

module Control.Fold
  ( Fold
  , stepFold
  , unfoldFold
  , unfoldFold_
  , foldl
  , scanl
  , mconcat
  , head
  , last
  , null
  , length
  , and
  , or
  , any
  , all
  , sum
  , product
  , maximum
  , minimum
  , elem
  , notElem
  , distributed
  ) where

import Prelude
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend)
import Data.Distributive (class Distributive, cotraverse)
import Data.Foldable (class Foldable)
import Data.Function (applyFlipped)
import Data.HeytingAlgebra (ff, tt)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Profunctor (class Profunctor, dimap, lmap)
import Data.Profunctor.Closed (class Closed, closed)
import Data.Traversable (class Traversable)

-- | A left fold, which takes zero or more values of type `a` as input
-- | and produces output of type `b`.
newtype Fold a b = Fold { step :: a -> Fold a b, finish :: Unit -> b }

-- | Step a fold by providing a single input.
stepFold :: forall a b. a -> Fold a b -> Fold a b
stepFold a (Fold o) = o.step a

-- | Create a `Fold` by providing an initial state, a function which updates
-- | that state, and a function which produces output from a state.
unfoldFold :: forall s a b. s -> (s -> a -> s) -> (s -> b) -> Fold a b
unfoldFold s0 step finish = go s0
  where
    go :: s -> Fold a b
    go s = Fold { step: go <<< step s, finish: \_ -> finish s }

-- | Create a `Fold` by providing an initial state and a function which updates
-- | that state. This is a variant of `unfoldFold` where the output is the state
-- | itself.
unfoldFold_ :: forall a b. b -> (b -> a -> b) -> Fold a b
unfoldFold_ s0 step = unfoldFold s0 step id

-- | Run a `Fold` by providing a `Foldable` container of inputs, and then
-- | generating a single output. This is analogous to the `foldl` function from
-- | `Data.Foldable`.
foldl :: forall f a b. Foldable f => Fold a b -> f a -> b
foldl fold xs = extract (Foldable.foldl (\(Fold o) -> o.step) fold xs)

-- | Run a `Fold` by providing a `Traversable` container of inputs, and
-- | generating an output for each input. This is analogous to the `scanl` function from
-- | `Data.Traversable`.
scanl :: forall f a b. Traversable f => Fold a b -> f a -> f b
scanl fold xs = map extract (Traversable.scanl (\(Fold o) -> o.step) fold xs)

-- | Fold over entire collections of inputs, producing a collection of outputs.
distributed :: forall f a b. Distributive f => Fold a b -> Fold (f a) (f b)
distributed = dimap applyFlipped (flip cotraverse id) <<< closed

-- | `Fold` values in some `Monoid`.
mconcat :: forall m. Monoid m => Fold m m
mconcat = unfoldFold_ mempty append

-- | A `Fold` which remembers the first input.
head :: forall a. Fold a (Maybe a)
head = unfoldFold_ Nothing (\m a -> m <|> Just a)

-- | A `Fold` which keeps the last input.
last :: forall a. Fold a (Maybe a)
last = unfoldFold_ Nothing (\_ a -> Just a)

-- | A `Fold` which tests whether any inputs were seen.
null :: forall a. Fold a Boolean
null = unfoldFold_ true (\_ _ -> false)

-- | A `Fold` which counts its inputs.
length :: forall a s. Semiring s => Fold a s
length = unfoldFold_ zero (\n _ -> n + one)

-- | A `Fold` which tests if _all_ of its inputs were true
-- | (generalized to work with an arbitrary `HeytingAlgebra`).
and :: forall b. HeytingAlgebra b => Fold b b
and = unfoldFold_ tt conj

-- | A `Fold` which tests if _any_ of its inputs were true
-- | (generalized to work with an arbitrary `HeytingAlgebra`).
or :: forall b. HeytingAlgebra b => Fold b b
or = unfoldFold_ ff disj

-- | A `Fold` which tests if _all_ of its inputs satisfy some predicate
-- | (generalized to work with an arbitrary `HeytingAlgebra`).
all :: forall a b. HeytingAlgebra b => (a -> b) -> Fold a b
all pred = lmap pred and

-- | A `Fold` which tests if _any_ of its inputs satisfy some predicate
-- | (generalized to work with an arbitrary `HeytingAlgebra`).
any :: forall a b. HeytingAlgebra b => (a -> b) -> Fold a b
any pred = lmap pred or

-- | A `Fold` which computes the sum of its inputs
-- | (generalized to work with an arbitrary `Semiring`).
sum :: forall s. Semiring s => Fold s s
sum = unfoldFold_ zero add

-- | A `Fold` which computes the product of its inputs
-- | (generalized to work with an arbitrary `Semiring`).
product :: forall s. Semiring s => Fold s s
product = unfoldFold_ one mul

-- | A `Fold` which computes the maximum of its inputs
-- | (generalized to work with an arbitrary `Bounded` type).
maximum :: forall a. Bounded a => Fold a a
maximum = unfoldFold_ bottom max

-- | A `Fold` which computes the minimum of its inputs
-- | (generalized to work with an arbitrary `Bounded` type).
minimum :: forall a. Bounded a => Fold a a
minimum = unfoldFold_ top min

-- | A `Fold` which tests if a specific value appeared as an input.
elem :: forall a. Eq a => a -> Fold a Boolean
elem a = any (_ == a)

-- | A `Fold` which tests if a specific value did not appear as an input.
notElem :: forall a. Eq a => a -> Fold a Boolean
notElem a = all (_ /= a)

instance profunctorFold :: Profunctor Fold where
  dimap f g (Fold o) = Fold { step, finish }
    where
      step = f >>> o.step >>> dimap f g
      finish = o.finish >>> g

instance closedFold :: Closed Fold where
  closed f = unfoldFold (const f) (lift2 (flip stepFold)) (extract <<< _)

instance functorFold :: Functor (Fold a) where
  map f (Fold o) = Fold { step, finish }
    where
      step = o.step >>> map f
      finish = o.finish >>> f

instance applyFold :: Apply (Fold a) where
  apply (Fold f) (Fold x) = Fold { step, finish }
    where
      step a = apply (f.step a) (x.step a)
      finish u = (f.finish u) (x.finish u)

instance applicativeFold :: Applicative (Fold a) where
  pure b = done
    where
      done = Fold { step: \_ -> done, finish: \_ -> b }

instance extendFold :: Extend (Fold a) where
  extend f = map f <<< dup
    where
      dup fold@(Fold o) = Fold { step, finish }
        where
          step a = dup (o.step a)
          finish _ = fold

instance comonadFold :: Comonad (Fold a) where
  extract (Fold o) = o.finish unit

instance semigroupFold :: Semigroup b => Semigroup (Fold a b) where
  append = lift2 append

instance monoidFold :: Monoid b => Monoid (Fold a b) where
  mempty = pure mempty
