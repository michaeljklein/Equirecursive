module Data.Equirecursive.Function where

import Data.Exists
import Data.Equirecursive
import Control.Lens
import Data.Default

import Data.Kind
import Data.X.Pair
import Data.Equirecursive.Class

import Control.Category
import Control.Arrow
import Data.Profunctor
import Prelude hiding (id, (.))


-- - a $ function for RecurseL (a -> c XY), which is much more limited than an automatic push
--    Could even do something like:
--      ($) :: RecurseL (a -> c XY) -> a -> RecurseL (a -> c XY)
--      (supposing that we have a way to get inside `c`.)
-- - convert a function into one that allows reporting its arguments: Partial: partial
-- - an infinite-type based printf: Nary: naryl ++ Exists Show
-- - generic infinite-type functions: Nary: naryl, naryr



-- | A n-ary function. This is the first real bit of fun with equirecursive types!
newtype Nary (a :: Type) (b :: Type) = Nary { getNary :: RecurseL (a -> (b, XY)) }


instance Pull (Nary a0 b0) (Nary a1 b1) (a0 -> (b0, Nary a0 b0)) (a1 -> (b1, Nary a1 b1))


-- | Lazily apply a function to all the partial
-- results in an `Nary` function.
instance Functor (Nary a) where
  fmap = undefined


-- | Analogous to `Applicative` for functions:
-- @(`<*>`) f g x = f x (g x)@
instance Applicative (Nary a) where
  -- | constant function for nary. maybe (arr const)?
  pure :: b -> Nary a b
  pure = undefined

  -- | Hmmmmmmmm......
  (<*>) :: Nary a (b -> c) -> Nary a b -> Nary a c
  (<*>) = undefined


-- | Consider the instance for @((->) a)@:
-- @f >>= k = \ r -> k (f r) r@
instance Monad (Nary a) where
  return :: b -> Nary a b
  return = pure

  (>>=) :: Nary a b -> (b -> Nary a c) -> Nary a c
  (>>=) = undefined


-- | Consider making `Nary` also `Strong`.
instance Profunctor Nary where
  dimap :: (a -> b) -> (c -> d) -> Nary b c -> Nary a d
  dimap = undefined

  rmap :: (b -> c) -> Nary a b -> Nary a c
  rmap = fmap


-- | `id` should be a piece of cake. Less sure about @(.)@ though.
-- Hmmmmmmmm......
instance Category Nary where
  id :: Nary a a
  id = undefined

  (.) :: Nary b c -> Nary a b -> Nary a c
  (.) = undefined


-- | See function descriptions
instance Arrow Nary where
  -- | This should be the (almost most) trivial nary function:
  -- @arr f == pull (arr f) undefined@
  arr :: (b -> c) -> Nary b c
  arr = undefined

  -- | This makes me think that there should be a:
  -- @((a -> b) -> (c -> d)) -> Nary a b -> Nary c d@-like
  -- function?
  first :: Nary b c -> Nary (b, d) (c, d)
  first = undefined

  second :: Nary b c -> Nary (d, b) (d, c)
  second = undefined

  (***) :: Nary b c -> Nary b' c' -> Nary (b, b') (c, c')
  (***) = undefined

  (&&&) :: Nary b c -> Nary b c' -> Nary b (c, c')
  (&&&) = undefined


instance ArrowChoice Nary where
  -- | Feed marked inputs through the argument arrow, passing the rest through unchanged to the output.
  left :: Nary b c -> Nary (Either b d) (Either c d)
  left = leftApp
  -- Any instance of ArrowApply can be made into an instance of ArrowChoice by defining left = leftApp.


-- | Oh, this should be very possible. There's only one @b@ supplied
-- so it should be trivial.
instance ArrowApply Nary where
  app :: Nary (Nary b c, b) c
  app = undefined


-- | Not sure if this is possible
instance ArrowLoop Nary where
  loop :: Nary (b, d) (c, d) -> Nary b c
  loop = undefined


-- | Make a `foldr`-like n-ary function (unimplemented)
naryr :: (a -> b -> b) -> b -> Nary a b
naryr f x = undefined

-- | Make a `foldl`-like n-ary function (unimplemented)
naryl :: (b -> a -> b) -> b -> Nary a b
naryl = undefined

-- rec _ :: (     RecurseL (Int -> (Int, XY      )))
--     _ :: (              (Int -> (Int, RecurseV)))
--        ->(Int                                   )
--        ->(Int, RecurseU (Int -> (Int, RecurseV)))




-- | Equirecursively typed functions that support unapplying and accessing applied arguments.
newtype Partial (a :: Type) = Result { getPartial :: RecurseL (MakePartial a a) }

-- | Take a function and make one that allows accessing arguments when some, but not all,
-- have been applied. E.g. getting the @1@ out of @(`+` 1)@.
makePartial :: a -> Partial a
makePartial = undefined

-- | Somehow, make a class that allow accessing the result of a `Partial`ly applied function.
-- Quite probably, I should consider specifying this, based on currently applied args
-- (so that if a0, a1 are applied, it doesn't look like you can get a3.)
-- (Reconsider type)
partial :: Lens (a0 ->an ->Partial b) (a0 ->an ->Partial c) (MakePartial b b) (MakePartial c c)
partial = undefined

-- pull :: Partial (Int -> Bool) -> Int -> (Int .: Bool, Partial (Int -> Bool))

-- pull :: Partial (Int -> Bool -> ()) -> Int -> Bool -> (Int .: Bool .: (), Partial (Int -> Bool -> ()))

-- getArgs :: (Bool -> (Int .: Bool .: (), Result (Int -> Bool -> ()))) -> (Int        )
-- getArgs :: (        (Int .: Bool .: (), Result (Int -> Bool -> ()))) -> (Int .: Bool)

-- makeResult :: (Int -> Bool) -> Result (Int -> Bool)

-- | This goes like: @MakePartial (Int -> Bool -> ()) = Int -> Bool -> (Int .: Bool .: (), XY)@
type family MakePartial (a :: Type) (b :: Type) :: Type where
  MakePartial a (b -> c) = b -> MakePartial a c
  MakePartial a (b     ) = (UnFunc a, XY)

-- | Used to get the result type of a function. Should satisfy:
-- @
-- IsAtom a ==> UnFunc a == a
-- UnFunc a == UnFunc (t -> a)
-- @
type family UnFunc (a :: Type) :: Type where
  UnFunc (a -> b) = a .: UnFunc b
  UnFunc (a     ) = a

-- SUCCESS!!!!!!!!
-- λ> let tryPull = (unsafeCoerce :: RecurseL (Int -> (Int, XY)) -> Int -> (Int, RecurseL (Int -> (Int, XY))))
-- λ> let try = (\f x -> (x, return (f . (+x)))) :: (Int -> (Int, RecurseV)) -> (Int -> (Int, RecurseU (Int -> (Int, RecurseV))))
-- λ> let tryg = rec try :: RecurseL (Int -> (Int, XY))
-- λ> fst $ tryPull (snd $ (tryPull . snd $ tryPull tryg 1) 2) 0
-- 3

-- UnFunc (a -> b -> c -> d -> e) = a .: b .: c .: d .: e


-- class Resulting c s t a b | s -> a, t -> b, s b -> t, t a -> s where
--   resulting :: Lens s t (Result c a) (Result c b)

-- instance (Default d, Resulting c s t a b) => Resulting c (d -> s) (d -> t) a b where
--   -- resulting :: Lens (     s) (     t) a b
--   -- resulting :: Lens (d -> s) (d -> t) a b
--   -- resulting :: (a -> f b) -> (     s) -> f (     t)
--   -- resulting :: (a -> f b) -> (d -> s) -> f (d -> t)
--   resulting f = undefined


-- (a0 -> .. -> an) -> (a0 -> .. -> Result c an)
