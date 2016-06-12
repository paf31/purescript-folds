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
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.HeytingAlgebra (ff, tt)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Profunctor (class Profunctor, dimap, lmap)
import Data.Traversable (class Traversable)
import Data.Traversable as Traversable

data Fold a b = Fold { step :: a -> Fold a b, finish :: Unit -> b }

stepFold :: forall a b. a -> Fold a b -> Fold a b
stepFold a (Fold o) = o.step a

unfoldFold :: forall s a b. s -> (s -> a -> s) -> (s -> b) -> Fold a b
unfoldFold s0 step finish = go s0
  where
    go :: s -> Fold a b
    go s = Fold { step: go <<< step s, finish: \_ -> finish s }

unfoldFold_ :: forall a b. b -> (b -> a -> b) -> Fold a b
unfoldFold_ s0 step = unfoldFold s0 step id

foldl :: forall f a b. Foldable f => Fold a b -> f a -> b
foldl fold xs = extract (Foldable.foldl (\(Fold o) -> o.step) fold xs)

scanl :: forall f a b. Traversable f => Fold a b -> f a -> f b
scanl fold xs = map extract (Traversable.scanl (\(Fold o) -> o.step) fold xs)

mconcat :: forall m. Monoid m => Fold m m
mconcat = unfoldFold_ mempty append

head :: forall a. Fold a (Maybe a)
head = unfoldFold_ Nothing (\m a -> m <|> Just a)

last :: forall a. Fold a (Maybe a)
last = unfoldFold_ Nothing (\_ a -> Just a)

null :: forall a. Fold a Boolean
null = unfoldFold_ true (\_ _ -> false)

length :: forall a s. Semiring s => Fold a s
length = unfoldFold_ zero (\n _ -> n + one)

and :: forall b. HeytingAlgebra b => Fold b b
and = unfoldFold_ tt conj

or :: forall b. HeytingAlgebra b => Fold b b
or = unfoldFold_ ff disj

all :: forall a b. HeytingAlgebra b => (a -> b) -> Fold a b
all pred = lmap pred and

any :: forall a b. HeytingAlgebra b => (a -> b) -> Fold a b
any pred = lmap pred or

sum :: forall s. Semiring s => Fold s s
sum = unfoldFold_ zero add

product :: forall s. Semiring s => Fold s s
product = unfoldFold_ one mul

maximum :: forall a. Bounded a => Fold a a
maximum = unfoldFold_ bottom max

minimum :: forall a. Bounded a => Fold a a
minimum = unfoldFold_ top min

elem :: forall a. Eq a => a -> Fold a Boolean
elem a = any (_ == a)

notElem :: forall a. Eq a => a -> Fold a Boolean
notElem a = all (_ /= a)

instance profunctorFold :: Profunctor Fold where
  dimap f g (Fold o) = Fold { step, finish }
    where
      step = f >>> o.step >>> dimap f g
      finish = o.finish >>> g

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
