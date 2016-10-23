{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}

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

import Control.Lens.Internal.Setter
import Data.X
import Unsafe.Coerce
import Data.Function (fix)
import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality
import Data.Profunctor.Unsafe
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


class IsPartial a
-- | once arg0..argN are applied, those args are now accessable, but let's just make it the most recent arg
--   the final arg is accessable on its own
--   args may be unapplied
--   the original function may be recovered
--   unsafeCoerce should not be used on Partial, unless used inside the wrapper?
--   (-$) :: a -> (UnApp a -> a)
--   the original function may be replaced, cleanly (args are applied to the new function)
--   args can be applied to the original function?
--
--   Oh, probably also want to use Y for this one, at least for the last arg?

-- class Partialable a

class Arg (n :: Nat) s t a b | n s -> a, n t -> b, n s b -> t, n t a -> s where
  arg :: X n -> Lens s t a b

instance Arg 0 s t s t where
  arg _ = id

instance Arg (n - 1) s t a b => Arg n s t a b where
  arg = undefined


type family MakePartial (a :: Type) (b :: Type) :: Type where
  MakePartial a (b -> c) = b ->  MakePartial a c
  MakePartial a (X    c) = (ArgsOf a, c, a, XY)
  MakePartial a (     c) = (ArgsOf a, c, a, XY)

type family ArgsOf (a :: Type) :: Type where
  ArgsOf (a -> b) = a :. ArgsOf b
  ArgsOf (     b) =      VoidX

type family ResultOf (a :: Type) :: Type where
  ResultOf (a -> b) = ResultOf b
  ResultOf (     b) =          b

type family (~>) (a :: Type) (r :: Type) :: Type where
  (~>) (a -> b) r = a -> b ~> (a :. r)
  (~>) (Y a   ) r =       a :. r
  (~>) (  a   ) r =       a :. r

type family AssocL (a :: Type) :: Type where
  AssocL (a :. (b :. c)) = AssocL ((a :. b) :. c)
  AssocL  a              = a

assocL :: a -> AssocL a
assocL = unsafeCoerce

class Result s t a b | s -> a, t -> b, s b -> t, t a -> s where
  type HasDefaults s t a b :: Constraint
  result       :: HasDefaults s t a b => Lens s t a b
  setResult    :: Setter s t a b
  unsafeResult :: Lens s t a b

instance Result (Y a) (Y b) a b where
  type HasDefaults (Y a) (Y b) a b = ()
  result       f = fmap return . f . extract
  setResult    f = pure . fmap (untainted . f)
  unsafeResult f = fmap return . f . extract

instance Result s t a b => Result (c -> s) (c -> t) a b where
  type HasDefaults (c -> s) (c -> t) a b = (HasDefaults s t a b, Default c)
  result       f = dimap ($ def) (fmap const) (result f)
  setResult    f = pure . fmap (untainted . setResult f)
  unsafeResult f = dimap ($ undefined) (fmap const) (unsafeResult f)


-- | Convert a function to one that returns both the original result
-- and all of its arguments.
class LogFunction (a :: Type) (b :: Type) | a -> b, b -> a where
  logFunction :: a -> b

instance LogFunction (Y a) (Y a) where
  logFunction :: Y a -> Y a
  logFunction = id

instance (LogFunction a b, Result s a b1 (t .: b1)) => LogFunction (t -> s) (t -> b) where
  logFunction :: (t -> s) -> (t -> b)
  logFunction f x = logFunction $ (setResult %~ (x :.:)) f x


-- | Drop an argument from a logged function.
-- Safe iff the function is lazy.
unsafeDropArg :: Result s t (a .: b) b => (a -> s) -> t
unsafeDropArg = (unsafeResult %~ (\(_ :.: y) -> y)) . ($ undefined)

-- | Drop an argument from a logged function.
dropArg :: (Result s t (a .: b) b, Default a, HasDefaults s t (a .: b) b) => (a -> s) -> t
dropArg = (result %~ (\(_ :.: y) -> y)) . ($ def)


-- | Get the last applied argument from a logged function.
class LastArg (a :: Type) (r :: Type) | a -> r where
  lastArg :: a -> r

instance LastArg (Y (r .: t)) r where
  lastArg :: Y (r .: t) -> r
  lastArg = (\(x :.: _) -> x) . extract

instance (LastArg t r, Result s t (a .: b) b, Default a, HasDefaults s t (a .: b) b) => LastArg (a -> s) r where
  lastArg :: (a -> s) -> r
  lastArg = lastArg . dropArg


-- | Get the last applied argument from a logged function.
-- Safe iff the function is lazy and safe.
class UnsafeLastArg (a :: Type) (r :: Type) | a -> r where
  unsafeLastArg :: a -> r

instance UnsafeLastArg (Y (r .: t)) r where
  -- lastArg       = (\(x :.: _) -> x) . extract
  unsafeLastArg = (\(x :.: _) -> x) . extract

instance (UnsafeLastArg t r, Result s t (a .: b) b) => UnsafeLastArg (a -> s) r where
  -- lastArg       = lastArg . dropArg
  unsafeLastArg = unsafeLastArg . unsafeDropArg



newtype Partial2 (a :: Type) = Partial2 { getPartial2 :: RecurseL (a ~> (a, XY)) }

data Partial (a :: Type) = IsPartial (MakePartial a a) => Partial { getPartial :: RecurseL (MakePartial a a) }

pullPartial' :: Iso (Partial s) (Partial t) (MakePartial' s s) (MakePartial' t t)
pullPartial' = unsafeCoerce

type family MakePartial' (a :: Type) (b :: Type) :: Type where
  MakePartial' a (b -> c) = b ->  MakePartial' a c
  MakePartial' a (     c) = (ArgsOf a, c, a, Partial a)

instance Pull
  (Partial (a0 -> Y b))
  (Partial (a0' -> Y b'))
  (a0  -> (a0  :. VoidX, Y b , a0  -> Y b , Partial (a0  -> Y b )))
  (a0' -> (a0' :. VoidX, Y b', a0' -> Y b', Partial (a0' -> Y b')))

voidX :: VoidX
voidX = error "VoidX"

class PartialMake (a :: Type) where
  makePartial :: a -> Partial a

instance PartialMake (a -> Y b) where
  makePartial :: (a -> Y b) -> Partial (a -> Y b)
  makePartial f = (\x0 -> (x0 :. voidX, f x0, f, makePartial f)) ^. push

instance PartialMake (a -> b -> Y c) where
  makePartial :: (a -> b -> Y c) -> Partial (a -> b -> Y c)
  makePartial f = (\x0 x1 -> (x0 :. (x1 :. voidX), f x0 x1, f, makePartial f)) ^. from pullPartial'

instance PartialMake (a -> b -> c -> Y d) where
  makePartial :: (a -> b -> c -> Y d) -> Partial (a -> b -> c -> Y d)
  makePartial f = (\x0 x1 x2 -> (x0 :. (x1 :. (x2 :. voidX)), f x0 x1 x2, f, makePartial f)) ^. from pullPartial'

instance PartialMake (a -> b -> c -> d -> Y e) where
  makePartial :: (a -> b -> c -> d -> Y e) -> Partial (a -> b -> c -> d -> Y e)
  makePartial f = (\x0 x1 x2 x3 -> (x0 :. (x1 :. (x2 :. (x3 :. voidX))), f x0 x1 x2 x3, f, makePartial f)) ^. from pullPartial'


class PartialApp s t a b | s -> a, t -> b, s b -> t, t a -> s where
  partialApp :: Iso s t (a, a -> s) (a, a -> t)
  partialApp = dimap unsafeCoerce (fmap unsafeCoerce)

instance PartialApp (Partial (a0 -> a1)) (Partial (b0 -> b1)) (a0, a0 -> Partial (a0 -> a1)) (b0, b0 -> Partial (b0 -> b1))

-- (             Partial (a0 -> a1            )) (a0, a0             -> Partial (a0 -> a1            ))
-- (             Partial (a0 -> a1 -> a2      )) (a1,       a1       -> Partial (a0 -> a1 -> a2      ))
-- ( a1       -> Partial (a0 -> a1 -> a2      )) (a0, a0 -> a1       -> Partial (a0 -> a1 -> a2      ))
-- (             Partial (a0 -> a1 -> a2 -> a3)) (a2,             a2 -> Partial (a0 -> a1 -> a2 -> a3))
-- (       a2 -> Partial (a0 -> a1 -> a2 -> a3)) (a1,       a1 -> a2 -> Partial (a0 -> a1 -> a2 -> a3))
-- ( a1 -> a2 -> Partial (a0 -> a1 -> a2 -> a3)) (a0, a0 -> a1 -> a2 -> Partial (a0 -> a1 -> a2 -> a3))

-- | given type, L: drop first arg, R: tuplify first arg

-- | Convert a list of types to a functional type,
-- e.g. @a .: b .: c@ to @a -> b -> c@
type family Apps (a :: Type) :: Type where
  Apps (a .: VoidX) = a
  Apps (a .: b    ) = a -> Apps b

-- n     = number of left  args
-- n + m = total     right args
type family AppPartial (n :: Nat) (m :: Nat) (as :: Type) :: Type where
  AppPartial n m as = AppsTo (((n+1) !.. (n+m+1)) as) (Partial (Apps ((0 !.. (n+m+2)) as)))

type family AppPartial' (n :: Nat) (m :: Nat) (as :: Type) :: Type where
  AppPartial' n m as = (as !! n, AppsTo ((n !.. (n+m+1)) as) (Partial (Apps ((0 !.. (n+m+2)) as))))

type AS a0 a1 a2 a3 a4 a5 a6 a7 = a0 .: a1 .: a2 .: a3 .: a4 .: a5 .: a6 .: a7 .: VoidX
type BS b0 b1 b2 b3 b4 b5 b6 b7 = b0 .: b1 .: b2 .: b3 .: b4 .: b5 .: b6 .: b7 .: VoidX

type family AppsTo (a :: Type) (b :: Type) :: Type where
  AppsTo (a .: b) c = a -> AppsTo b c
  AppsTo (VoidX ) c =               c
  AppsTo (a     ) c = TypeError ('Text "AppsTo")

type family LTrim (n :: Nat) (a :: Type) :: Type where
  LTrim 0 (a     ) = a
  LTrim n (a -> b) = LTrim (n-1) b

type family RTrim (n :: Nat) (a :: Type) :: Type where
  RTrim 0 (a -> b) = a
  RTrim n (a -> b) = a -> RTrim (n-1) b

-- So partialApp + PartialResult give 100% of the desired features!
-- Even better, partialResult should be contained somehow insside partialApp...
--
-- TODO:
-- Ok, going to reduce the functionality a bit: there is a Lens to the function, a Setter to the result, and an Iso to the last arg (below)
-- RecurseL (a -> b -> (a .: b, c, a -> b -> c, XY))?
-- RecurseL (a -> b -> PartialResult (a -> b -> c) XY)?
--
-- instance Pull                (Partial (a  -> b  -> c ))
--                              (Partial (a' -> b' -> c'))
--   (a  -> b  -> (a  .: b , c , Partial (a  -> b  -> c )))
--   (a' -> b' -> (a' .: b', c', Partial (a' -> b' -> c'))) where
--   pull = pullPartial
--
-- instance Pull (Partial s) (Partial t) (PulledPartial s) (PulledPartial t)
--
-- Partial (a -> b -> c) <-> a -> b -> (a .: b, c, Partial (a -> b -> c))
-- instance Eq (ResultOf a) => Eq (Partial a) where p1 == p2 = resultOf p1 == resultOf p2
-- Instance Ord (ResultOf a) => Ord (Partial a) where p1 `compare` p1 = resultOf p1 `compare` resultOf p2
-- instance Show (ResultOf a) => Show (Partial a) where show p = show (resultOf p)
-- instance Num (ResultOf a) => Num (Partial a)
--
-- (Partial (a -> b -> c)) (a, a -> (b, b -> Partial (a -> b -> c)))
-- (Partial (a -> b)) (a, a -> Partial (a -> b))
-- (b -> Partial (a -> b -> c)) (a, a -> b -> Partial (a -> b -> c))
--
-- Iso (Partial (a -> b)) (Partial (a' -> b')) (a, a -> Partial (a -> b)) (a', a' -> Partial (a' -> b'))
-- Iso (t0 -> Partial (a -> t0 -> b)) (t0 -> Partial (a' -> t0 -> b')) (a, a -> t0 -> Partial (a -> t0 -> b)) (a', a' -> t0 -> Partial (a' -> t0 -> b'))
-- Iso (Partial (a -> t0 -> b)) (Partial (a' -> t0' -> b')) (t0, t0 -> Partial (a -> t0 -> b)) (t0', t0' -> Partial (a' -> t0' -> b'))
--  If Iso' t0 t0', can remap t0. (new iso, that remaps most recent arg)
--

-- maybe like this?
-- instance PartialApp (Partial (a -> b)) (Partial (c -> d)) a c where

class PartialResult s t a b | s -> a, a -> s, t -> b, b -> t, s b -> t, t a -> s where
  partialResult :: Iso (Partial s) (Partial t) a b

-- Iso (

-- class PartialArg ?
--   we can set applied or unapplied args
--   we can map over


-- instance PartialResult (a -> b -> c) (a -> b -> c) c c where result :: Iso' (a -> b -> Partial (a -> b -> c)) (Partial (a -> b -> c))
-- Partiable a => Partial { getFunction :: a, getResult :: ResultOf a, getArgs :: ArgsOf a }

-- makePartial :: a -> ResultMap (Partial a) a

-- -- unApply = apply known args to function
-- -- lastArg = first from tuple
-- -- getFunction = from Partial
-- -- getResult = from Partial

-- (a -> b -> c) -> (a -> b -> Partial (a -> b -> c))
-- Partial (a -> b -> c) -> a -> b -> Partial (a -> b -> c)
-- (b -> Partial (a -> b -> c)) -> a
-- Partial (a -> b -> c) -> b
-- (b -> Partial (a -> b -> c)) -> a -> b -> Partial (a -> b -> c)
-- Partial (a -> b -> c) -> b -> Partial (a -> b -> c)
-- Partial (a -> b -> c) -> c


-- makePartial :: a -> Partial a


-- a -> b
-- RecurseL (a -> (a .: b, XY))

-- a -> b -> c
-- RecurseL (a -> b -> (a .: b .: c, XY))

-- a -> b -> c -> d
-- RecurseL (a -> b -> c -> (a .: b .: c .: d, XY))

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


