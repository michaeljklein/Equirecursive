{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Equirecursive.Function where

import Data.Exists
import Data.Equirecursive
import Control.Lens
import Data.Default

import Data.Kind
import Data.X.Pair
import Data.Equirecursive.Class
import Data.Recurse

import Control.Category
import Control.Arrow
import Data.Profunctor
import Prelude hiding (id, (.))
import Control.Monad.Zip
import Control.Comonad
import Control.Monad

import Data.X
import Unsafe.Coerce
import Data.Function (fix)
import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality

-- type family F (c :: Constraint) (a :: Type) :: Constraint where
--   F (a ~ (a, a)) a = a ~ a

-- tt :: (c => a) -> (F c a => a)
-- tt = undefined

-- newtype List a = List { getList :: RecurseL (Y a, XY) }

-- instance Pull1 List (Y a, XY) (Y b, XY) a b where
--   pull1 :: Lens (List a) (List b) (a, List a) (b, List b)
--   pull1 = undefined

-- pullN :: RecurseL a -> P (RecurseL a) a
-- pullN = undefined

--  The other reuirements for this include: updating rec methods, updating equality, etc.
-- λ> :t pullN (undefined :: RecurseL (Y a, XY))
--   ∷ (a, Recurse 'Locked (Y a, XY))
--
-- λ> :t pullN (undefined :: RecurseL (Y a -> (Y b, XY)))
--   ∷ a → (b, Recurse 'Locked (Y a → (Y b, XY)))

-- instead of RecurseL (a, XY), do RecurseL (Y a, XY). Then `P` can manage the polymorphism!!!


-- TODO: Move Nary and Partial into their own modules


-- OOOH, I think I can solve part of the type equality within
-- MapX, UnfoldX, etc. It will only work for prepared cases,
-- but I should be able to make an AnyType newtype that I can
-- safely coerce to/from. Ideally, it would have the NotXY constraint,
-- but I think I can make it work anyway, and possibly even incorporate
-- the NotXY constraint to cover some cases. Hmmmmmm.... Recursive injectivity would be very nice.


-- | Need to check how `XY` is recognized in other places.
-- I think there are a couple cases where @`X` (`X` :: `Nat` -> `Type`)@
-- would be recognized as `XY`, "just to be safe".
--
-- Yay for when injectivity actually works!
--
-- Note: all this is cool, and I should definately apply it to
-- theorems about arbitrary RecurseL's, but I think this specifically is
-- actually all covered by (==)!
type family NotXY1 (a :: Type) = (c :: Constraint) | c -> a where
  NotXY1 XY = () ~ XY
  NotXY1 a  = a  ~ a

-- | So this is neat, it's possible to use staged checks
-- to narrow down the search space. This family essentially says:
-- "Hey, ya know how XY :: Type? Well so then like everything else
-- can't be XY."
type family NotXY4 (a :: k   ) :: Constraint where
  NotXY4 (a :: Type) = NotXY3 a
  NotXY4 (a :: k   ) = a ~ a

-- | This checks whether the type is the result of exactly one
-- application, throwing out things like @`MaybeT` m a@, without
-- even knowing @m@ or @a@! Also helpful things like @(a, b)@.
type family NotXY3 (a :: Type) :: Constraint where
  NotXY3 (a b c) = (a b c) ~ (a b c)
  NotXY3 (a b) = NotXY2 (a b)
  NotXY3 (a  ) = a ~ a

-- | This checks whether the type is `X` applied
-- to something.
type family NotXY2 (a :: Type) :: Constraint where
  NotXY2 (X a) = NotXY1 (X a)
  NotXY2 (  a) = a ~ a

-- class ((s == XY) ~ eq0, (t == XY) ~ eq1) => CoerceRecTuple s t a b eq0 eq1 where
--   coerceRec :: RecurseL (s, t) -> (a, b)
--   coerceRec = unsafeCoerce

-- instance CoerceRecTuple XY XY (RecurseL (XY, XY)) (RecurseL (XY, XY)) 'True 'True where
--   coerceRec :: RecurseL (XY, XY) -> (RecurseL (XY, XY), RecurseL (XY, XY))

-- -- | Add constraint CoerceRec s
-- instance ((s == XY) ~ 'False) => CoerceRecTuple s XY s (RecurseL (s, XY)) 'False 'True where
--   coerceRec :: RecurseL (s , XY) -> (s                , RecurseL (s , XY))

-- -- | Add constraint CoerceRec t
-- instance ((t == XY) ~ 'False) => CoerceRecTuple XY t (RecurseL (XY, t)) t 'True 'False where
--   coerceRec :: RecurseL (XY, t ) -> (RecurseL (XY, t ), t                )

-- instance ((s == XY) ~ 'False, (t == XY) ~ 'False) => CoerceRecTuple s t s t 'False 'False where
--   coerceRec :: RecurseL (s , t ) -> (s                , t                )


-- | TODO: These classes play loose with unsafeCoerce. That shouldn't happen.
-- Should probably make a `NotXY` constraint and sprinkle it around these modules
-- liberally. That, plus adding proofs for `rec`/`pull` on these types should make
-- everything a bit safer.
--
-- Idea for that:
-- NotXY a => EList a :~: (a, EList a)
-- NotXY a => RecurseL (a, XY) :~: (a, RecurseL (a, XY))
--
-- Can probably TemplateHaskell it up a bit to make instances for a number of datatypes.
-- Hmm... What about a general interface?
-- NotXY a => CoerceXY a
-- CoerceXY a => CoerceXY (a, XY)
-- CoerceXY a => CoerceXY (XY, a)
--
-- It seems possible that I could make it work for a number of simpler types. The trick is
-- to make sure they resolve nicely when instantiated.
--
--
-- Also, should remember to make rec' that's determined more by argument than result, like rec is.
-- rec' should probably fail with a TypeError.


-- - a $ function for RecurseL (a -> c XY), which is much more limited than an automatic push
--    Could even do something like:
--      ($) :: RecurseL (a -> c XY) -> a -> RecurseL (a -> c XY)
--      (supposing that we have a way to get inside `c`.)
-- - convert a function into one that allows reporting its arguments: Partial: partial
-- - an infinite-type based printf: Nary: naryl ++ Exists Show
-- - generic infinite-type functions: Nary: naryl, naryr


-- | It works! `aP` effectively has the following type:
-- @
-- `aP` :: a -> a -> .. -> a -> `Nary` a b
-- @
class Ap (a :: Type) (b :: Type) (to :: Type) | to -> a, to -> b where
  aP :: Nary a b -> to

-- | Base case
instance Ap (a :: Type) b (Nary a b) where
  aP :: Nary a b -> Nary a b
  aP = id

-- | Inductive case
instance Ap a b to => Ap (a :: Type) b (a -> to) where
  aP :: Nary a b -> (a -> to)
  aP = fmap (aP . snd) . (^. pull)


class ApE (c :: k) (b :: Type) (to :: Type) | to -> c, to -> b where
  aPE :: Nary (Exists c) b -> to

instance ApE c b (Nary (Exists c) b) where
  aPE :: Nary (Exists c) b -> Nary (Exists c) b
  aPE = id

instance (ApE c b to, Constraints c a) => ApE c b (a -> to) where
  aPE :: Nary (Exists c) b -> (a -> to)
  aPE = dimap (ExistsK . return) (aPE . snd) . (^. pull)


-- | A n-ary function. This is the first real bit of fun with equirecursive types!
newtype Nary (a :: Type) (b :: Type) = Nary { getNary :: RecurseL (a -> (b, XY)) }

-- | This is the key coercion to make a `Nary`
naryCoerce :: ((a -> (b, RecurseV)) -> a -> (b, RecurseU (a -> (b, RecurseV)))) -> ((a -> (b, XY)) -> (a -> (b, XY)))
naryCoerce = unsafeCoerce

instance Pull (Nary a0 b0) (Nary a1 b1) (a0 -> (b0, Nary a0 b0)) (a1 -> (b1, Nary a1 b1))


-- | @`pure` `def`@
instance Default b => Default (Nary a b) where
  def = pure def


-- | Lazily apply a function to all the partial
-- results in an `Nary` function.
instance Functor (Nary a) where
  fmap = rmap

-- | Analogous to `Applicative` for unary functions.
instance Applicative (Nary a) where
  -- | Constant function for `Nary`.
  pure :: b -> Nary a b
  pure x = (const (x, pure x)) ^. push

  -- | I believe this is analogous to the @(`<*>`)@
  -- for @((->) a)@. Translate
  -- @`mzipWith` (`$`) f g@
  -- to
  -- @`fmap` (`uncurry` (`$`)) $ f *** g@.
  (<*>) :: Nary a (b -> c) -> Nary a b -> Nary a c
  (<*>) = mzipWith ($)


-- | Consider the instance for @((->) a)@:
-- @f >>= k = \ r -> k (f r) r@
instance Monad (Nary a) where
  return :: b -> Nary a b
  return = pure

  (>>=) :: Nary a b -> (b -> Nary a c) -> Nary a c
  (>>=) x f = join (f <$> x)
    where
      join :: Nary a (Nary a b) -> Nary a b
      join = pull %~ (\f x -> bimap (fst . ($ x) . (^. pull)) join $ f x)


-- | Equirecursive types often have a natural
-- `Comonad` instance.
instance Default a => Comonad (Nary a) where
  extract :: Nary a b -> b
  extract = fst . flip (^. pull) def

  duplicate :: Nary a b -> Nary a (Nary a b)
  duplicate = pull %~ ((fmap duplicate . join (,) . snd) .)


-- | Consider making `Nary` also `Strong`.
instance Profunctor Nary where
  dimap :: (a -> b) -> (c -> d) -> Nary b c -> Nary a d
  dimap f g = pull %~ (dimap f . bimap g . dimap f $ g)

  lmap :: (a -> b) -> Nary b c -> Nary a c
  lmap f = pull %~ (dimap f . second . lmap $ f)

  rmap :: (b -> c) -> Nary a b -> Nary a c
  rmap g = pull %~ (rmap . bimap g . rmap $ g)


instance Category Nary where
  id :: Nary a a
  id = (flip (,) id) ^. push

  (.) :: Nary b c -> Nary a b -> Nary a c
  (.) n0 n1 = n2 ^. push
    where
      n2 x = (z, zs . ys)
        where
          (y, ys) = (n1 ^. pull) x
          (z, zs) = (n0 ^. pull) y

-- | See function descriptions
instance Arrow Nary where
  -- | This should be the (almost most) trivial nary function:
  -- @arr f == pull (arr f) undefined@
  arr :: (b -> c) -> Nary b c
  arr f = (\x -> (f x, arr f)) ^. push

  first :: Nary b c -> Nary (b, d) (c, d)
  first = pull %~ (rmap (\((x, n), y) -> ((x, y), first n)) . first)

  second :: Nary b c -> Nary (d, b) (d, c)
  second = pull %~ (rmap (\(x, (y, n)) -> ((x, y), second n)) . second)

  (***) :: Nary b c -> Nary b' c' -> Nary (b, b') (c, c')
  (***) n0 n1 = (^. push) . rmap (\((x, xs), (y, ys)) -> ((x, y), xs *** ys)) $ n0 ^. pull *** n1 ^. pull

  (&&&) :: Nary b c -> Nary b c' -> Nary b (c, c')
  (&&&) n0 n1 = (^. push) . rmap (\((x, xs), (y, ys)) -> ((x, y), xs &&& ys)) $ n0 ^. pull &&& n1 ^. pull


instance ArrowChoice Nary where
  left :: Nary b c -> Nary (Either b d) (Either c d)
  left = leftApp


-- | Oh, this should be very possible. There's only one @b@ supplied
-- so it should be trivial.
instance ArrowApply Nary where
  app :: Nary (Nary b c, b) c
  app = arr . uncurry $ (fst .) . (^. pull)


-- | Not sure if this is possible, but it looks like it is!
instance ArrowLoop Nary where
  loop :: Nary (b, d) (c, d) -> Nary b c
  loop = pull %~ (\f b -> let ((c, d), n) = f (b, d) in (c, loop n))


instance MonadZip (Nary a) where
  mzip :: Nary a b -> Nary a c -> Nary a (b, c)
  mzip = (&&&)


-- | Make a `foldr`-like n-ary function
naryr :: (a -> b -> b) -> b -> Nary a b
naryr f x = (\y ->let z = f y x in (z, naryr f z)) ^. push

-- | Make a `foldl`-like n-ary function
naryl :: (b -> a -> b) -> b -> Nary a b
naryl f x = (\y ->let z = f x y in (x, naryl f z)) ^. push

naryCons :: Nary a [a]
naryCons = naryr (:) []

narym :: Monoid m => Nary m m
narym = naryr mappend mempty







-- -- | Equirecursively typed functions that support unapplying and accessing applied arguments.
-- newtype Partial (a :: Type) = Result { getPartial :: RecurseL (MakePartial a a) }

-- -- | Take a function and make one that allows accessing arguments when some, but not all,
-- -- have been applied. E.g. getting the @1@ out of @(`+` 1)@.
-- makePartial :: a -> Partial a
-- makePartial = undefined

-- -- | Somehow, make a class that allow accessing the result of a `Partial`ly applied function.
-- -- Quite probably, I should consider specifying this, based on currently applied args
-- -- (so that if a0, a1 are applied, it doesn't look like you can get a3.)
-- -- (Reconsider type)
-- partial :: Lens (a0 ->an ->Partial b) (a0 ->an ->Partial c) (MakePartial b b) (MakePartial c c)
-- partial = undefined

-- -- pull :: Partial (Int -> Bool) -> Int -> (Int .: Bool, Partial (Int -> Bool))

-- -- pull :: Partial (Int -> Bool -> ()) -> Int -> Bool -> (Int .: Bool .: (), Partial (Int -> Bool -> ()))

-- -- getArgs :: (Bool -> (Int .: Bool .: (), Result (Int -> Bool -> ()))) -> (Int        )
-- -- getArgs :: (        (Int .: Bool .: (), Result (Int -> Bool -> ()))) -> (Int .: Bool)

-- -- makeResult :: (Int -> Bool) -> Result (Int -> Bool)

-- -- | This goes like: @MakePartial (Int -> Bool -> ()) = Int -> Bool -> (Int .: Bool .: (), XY)@
-- type family MakePartial (a :: Type) (b :: Type) :: Type where
--   MakePartial a (b -> c) = b -> MakePartial a c
--   MakePartial a (b     ) = (UnFunc a, XY)

-- -- | Used to get the result type of a function. Should satisfy:
-- -- @
-- -- IsAtom a ==> UnFunc a == a
-- -- UnFunc a == UnFunc (t -> a)
-- -- @
-- type family UnFunc (a :: Type) :: Type where
--   UnFunc (a -> b) = a .: UnFunc b
--   UnFunc (a     ) = a


